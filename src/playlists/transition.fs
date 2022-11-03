module Aornota.Fap.Playlists.Transition

open Aornota.Fap.Playlists.Model
open Aornota.Fap.Persistence
open Aornota.Fap.Utilities
open Elmish
open LibVLCSharp.Shared
open System
open System.IO

type Direction =
    | Left
    | Right
    | Up
    | Down

type RelativePosition =
    | Above
    | Below

type ExternalMsg =
    | NotifyPlaylistsChanged
    | NotifyTrackStateChanged
    | NotifyMutedToggled
    | NotifyVolumeChanged
    | NotifyError of string

type Msg =
    // Internal
    | NoOp
    | ReadPlaylists of PlaylistId list * TrackId option * bool * Playlist list
    | ReadPlaylistError of ReadError * PlaylistId * PlaylistId list * TrackId option * bool * Playlist list
    | WritePlaylist of PlaylistId
    | DebounceWritePlaylistRequest of WritePlaylistRequestId * PlaylistId
    | WritePlaylistError of string * PlaylistId
    | DebounceSeekRequest of SeekRequestId * float32
    | RequestDefaultTrack of Playlist option * play: bool
    | RequestTrack of trackData: TrackData * Playlist * play: bool
    | RequestNoTrack
    // UI
    | OnSelectPlaylist of PlaylistId
    | OnMovePlaylist of PlaylistId * Direction
    | OnRemovePlaylist of PlaylistId
    | OnMoveTrack of TrackId * Direction
    | OnAddSubTotal of TrackId * RelativePosition
    | OnRemoveTrack of TrackId
    | OnRemoveSubTotal of SubTotalId
    | OnPlayTrack of TrackId
    | OnSeek of float32
    | OnPrevious
    | OnNext
    | OnPlay
    | OnPause
    | OnStop
    | OnToggleMuted
    | OnVolume of int
    // From MediaPlayer subscriptions
    | NotifyErrored
    | NotifyPlaying
    | NotifyPositionChanged of float32
    | NotifyEnded

[<Literal>]
let private DEBOUNCE_WRITE_PLAYLIST_REQUEST_DELAY = 250

[<Literal>]
let private DEBOUNCE_SEEK_REQUEST_DELAY = 250

// TODO-NMB: "Inline" at only call-site?...
let private playlist playlists playlistId =
    playlists |> List.tryFind (fun otherPlaylist -> otherPlaylist.Id = playlistId)

// TODO-NMB: "Inline" at only call-site?...
let private findSubTotal playlists subTotalId =
    let matches =
        playlists
        |> List.choose (fun playlist ->
            let subTotalMatches =
                playlist.Items
                |> List.choose (fun item ->
                    match item with
                    | SubTotal otherSubTotalId when otherSubTotalId = subTotalId -> Some item
                    | _ -> None)

            match subTotalMatches with
            | _ :: _ -> Some(playlist, subTotalMatches)
            | [] -> None)

    match matches with
    | [ playlist, subTotalMatches ] ->
        match subTotalMatches with
        | [ _ ] -> Ok playlist
        | [] -> Error $"no {nameof (SubTotal)} matches for {subTotalId} for {nameof (Playlist)} {playlist.Name}"
        | _ -> Error $"multiple {nameof (SubTotal)} matches for {subTotalId} for {nameof (Playlist)} {playlist.Name}"
    | [] -> Error $"no {nameof (SubTotal)} matches for {subTotalId}"
    | _ -> Error $"{nameof (SubTotal)} matches for {subTotalId} for multiple {nameof (Playlist)}s"

let private previousAndNext playlist trackId =
    let tracks = tracks playlist

    match tracks |> List.tryFindIndex (fun trackData -> trackData.Id = trackId) with
    | Some index ->
        let previous = if index > 0 then Some tracks[index - 1] else None

        let next =
            if index < tracks.Length - 1 then
                Some tracks[index + 1]
            else
                None

        Ok(previous, next)
    | None -> Error $"no matches for {trackId} for {nameof (Playlist)} {playlist.Name}"

// TODO-NMB: "Inline" at only call-site?...
let private updatePlaylist playlist (trackData: TrackData) =
    let mutable updated = 0

    let newItems =
        playlist.Items
        |> List.map (fun item ->
            if isTrackId trackData.Id item then
                updated <- updated + 1
                Track trackData
            else
                item)

    if updated = 1 then
        Ok { playlist with Items = newItems }
    else if updated = 0 then
        Error $"no matches for {trackData.Id} for {nameof (Playlist)} {playlist.Name}"
    else
        Error $"multiple matches for {trackData.Id} for {nameof (Playlist)} {playlist.Name}"

let private updatePlaylists (playlists: Playlist list) (playlist: Playlist) =
    let mutable updated = 0

    let newPlaylists =
        playlists
        |> List.map (fun otherPlaylist ->
            if otherPlaylist.Id = playlist.Id then
                updated <- updated + 1
                playlist
            else
                otherPlaylist)

    if updated = 1 then
        Ok newPlaylists
    else if updated = 0 then
        Error $"no matches for {playlist.Id} for {nameof (Playlist)}s"
    else
        Error $"multiple matches for {playlist.Id} for {nameof (Playlist)}s"

let private playTrack (track: TrackData) (player: MediaPlayer) =
    let path = Path.Combine(track.Folder, track.Name)
    use libvlc = new LibVLC()
    use media = new Media(libvlc, path, FromType.FromPath)
    player.Play media |> ignore

let init playlistIds lastTrackId muted volume autoPlay (player: MediaPlayer) =
    player.Media <- null

    { Playlists = []
      SelectedPlaylistId = None
      Muted = muted
      Volume = volume
      TrackState = None
      WritePlaylistRequests = []
      SeekRequests = [] },
    [ RequestNoTrack; ReadPlaylists(playlistIds, lastTrackId, autoPlay, []) ]

(* Notes:
    -- If change TrackState.Track (including setting TrackState to None) or TrackState.PlayerStatus, call (external) NotifyTrackStateChanged.
    -- If add / remove / reorder Playlists, call (external) NotifyPlaylistsChanged.
    -- If add new Playlist, call (external) NewPlaylistAdded?
    -- If add / remove / reorder Items for Playlist, call (internal) WritePlaylist. *)

let transition msg state (player: MediaPlayer) =
    let notifyError error =
        state, Cmd.none, [ NotifyError $"Playlists.transition -> {error}" ]

    let noChange = state, Cmd.none, []

    match msg with
    // Internal
    | NoOp -> noChange
    | ReadPlaylists (playlistIds, lastTrackId, autoPlay, playlists) ->
        match playlistIds with
        | playlistId :: playlistIds ->
            let read () =
                async { return! readPlaylist playlistId }

            let handleResult =
                function
                | Ok playlist ->
                    ReadPlaylists(playlistIds, lastTrackId, autoPlay, (playlist :: (playlists |> List.rev)) |> List.rev)
                | Error error -> ReadPlaylistError(error, playlistId, playlistIds, lastTrackId, autoPlay, playlists)

            state, Cmd.OfAsync.perform read () handleResult, []
        | [] ->
            let lastTrack =
                match lastTrackId with
                | Some lastTrackId ->
                    match findTrack playlists lastTrackId with
                    | Ok (playlist, trackData) ->
                        Some(playlist.Id, Cmd.ofMsg (RequestTrack(trackData, playlist, autoPlay))), []
                    | Error error -> None, [ NotifyError $"Playlists.transition -> {error}" ]
                | None -> None, []

            let selectedPlaylistId, cmd, externalMsgs =
                match lastTrack with
                | (Some selectedPlaylistIdAndMsg, externalMsgs) ->
                    Some(fst selectedPlaylistIdAndMsg), snd selectedPlaylistIdAndMsg, externalMsgs
                | (None, externalMsgs) -> None, Cmd.ofMsg (RequestDefaultTrack(None, autoPlay)), externalMsgs

            { state with
                Playlists = playlists
                SelectedPlaylistId = selectedPlaylistId },
            cmd,
            NotifyPlaylistsChanged :: externalMsgs
    | ReadPlaylistError (readError, PlaylistId guid, playlistIds, lastTrackId, autoPlay, playlists) ->
        state,
        Cmd.ofMsg (ReadPlaylists(playlistIds, lastTrackId, autoPlay, playlists)),
        [ NotifyError $"Playlists.transition -> Unable to read {nameof (Playlist)} ({guid}): {readErrorText readError}" ]
    | WritePlaylist playlistId ->
        let writePlaylistRequest = WritePlaylistRequestId.Create(), playlistId

        let delay () =
            async {
                do! Async.Sleep DEBOUNCE_WRITE_PLAYLIST_REQUEST_DELAY
                return writePlaylistRequest
            }

        { state with WritePlaylistRequests = writePlaylistRequest :: state.WritePlaylistRequests },
        Cmd.OfAsync.perform delay () DebounceWritePlaylistRequest,
        []
    | DebounceWritePlaylistRequest (writePlaylistRequestId, playlistId) ->
        let newWritePlaylistPlayerRequests =
            state.WritePlaylistRequests
            |> List.filter (fun (otherWritePlaylistRequestId, _) ->
                otherWritePlaylistRequestId <> writePlaylistRequestId)

        match
            newWritePlaylistPlayerRequests
            |> List.filter (fun (_, otherPlaylistId) -> otherPlaylistId = playlistId)
        with
        | _ :: _ -> { state with WritePlaylistRequests = newWritePlaylistPlayerRequests }, Cmd.none, []
        | [] ->
            let writePlaylist playlistId =
                async {
                    match playlist state.Playlists playlistId with
                    | Some playlist -> return! writePlaylist playlist
                    | None -> return Error "Playlist not found"
                }

            let handleResult =
                function
                | Ok _ -> NoOp
                | Error error -> WritePlaylistError(error, playlistId)

            { state with WritePlaylistRequests = newWritePlaylistPlayerRequests },
            Cmd.OfAsync.perform writePlaylist playlistId handleResult,
            []
    | WritePlaylistError (error, PlaylistId guid) -> notifyError $"Unable to write playlist ({guid}): {error}"
    | RequestDefaultTrack (playlist, play) ->
        let playlist =
            match playlist with
            | Some playlist -> Some playlist
            | None ->
                match
                    state.Playlists
                    |> List.filter (fun playlist -> tracks playlist |> List.length > 0)
                with
                | playlist :: _ -> Some playlist
                | [] -> None

        match playlist with
        | Some playlist ->
            match tracks playlist with
            | trackData :: _ ->
                { state with SelectedPlaylistId = Some playlist.Id },
                Cmd.ofMsg (RequestTrack(trackData, playlist, play)),
                []
            | [] -> noChange
        | None -> noChange
    | RequestTrack (trackData, playlist, play) ->
        match previousAndNext playlist trackData.Id with
        | Ok (previous, next) ->
            let newStateAndCmdAndExternalMsgs =
                match state.TrackState with
                | Some trackState ->
                    if trackState.Track.Id = trackData.Id then
                        if not play then
                            Error
                                $"{nameof (RequestTrack)} when trackState is already requested {nameof (TrackData)} and not play"
                        else
                            match trackState.PlayerState with
                            | Playing _ -> Ok(state, Cmd.ofMsg (OnSeek START_POSITION), [])
                            | _ -> Ok(state, Cmd.ofMsg OnPlay, [])
                    else
                        if play then
                            playTrack trackData player
                        else
                            player.Media <- null

                        Ok(
                            { state with
                                TrackState =
                                    Some
                                        { Track = trackData
                                          PlayerState = (if play then AwaitingPlay else NoMedia)
                                          Previous = previous
                                          Next = next } },
                            Cmd.none,
                            [ NotifyTrackStateChanged ]
                        )
                | None ->
                    if play then
                        playTrack trackData player

                    Ok(
                        { state with
                            TrackState =
                                Some
                                    { Track = trackData
                                      PlayerState = (if play then AwaitingPlay else NoMedia)
                                      Previous = previous
                                      Next = next } },
                        Cmd.none,
                        [ NotifyTrackStateChanged ]
                    )

            match newStateAndCmdAndExternalMsgs with
            | Ok (newState, cmd, externalMsgs) -> newState, cmd, externalMsgs
            | Error error -> notifyError error
        | Error error -> notifyError $"{nameof (RequestTrack)}: {error}"
    | RequestNoTrack ->
        player.Media <- null

        { state with
            TrackState = None
            SeekRequests = [] },
        Cmd.none,
        [ NotifyTrackStateChanged ]
    | DebounceSeekRequest (seekRequestId, position) ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _ ->
                let newSeekRequests =
                    state.SeekRequests
                    |> List.filter (fun otherSeekRequestId -> otherSeekRequestId <> seekRequestId)

                match newSeekRequests with
                | _ :: _ -> { state with SeekRequests = newSeekRequests }, Cmd.none, []
                | [] ->
                    player.Position <- position

                    { state with
                        TrackState = Some { trackState with PlayerState = Playing(position, Some position) }
                        SeekRequests = newSeekRequests },
                    Cmd.none,
                    []
            | _ -> notifyError $"{nameof (DebounceSeekRequest)} when {nameof (PlayerState)} not {nameof (Playing)}"
        | None -> notifyError $"{nameof (DebounceSeekRequest)} when trackState is {nameof (None)}"
    // From UI
    | OnSelectPlaylist playlistId -> { state with SelectedPlaylistId = Some playlistId }, Cmd.none, []
    | OnMovePlaylist (playlistId, Left) ->
        match state.Playlists |> List.tryFindIndex (fun playlist -> playlist.Id = playlistId) with
        | Some index ->
            if index = 0 then
                notifyError
                    $"{nameof (OnMovePlaylist)} {nameof (Left)}: {nameof (Playlist)} {playlistId} already left-most"
            else
                let (_, newPlaylists, _) =
                    state.Playlists
                    |> List.fold
                        (fun (i, newPlaylists, pending) playlist ->
                            let newPlaylists, pending =
                                match pending with
                                | Some pending -> pending :: playlist :: newPlaylists, None
                                | None when i = index - 1 -> newPlaylists, Some playlist
                                | None -> playlist :: newPlaylists, None

                            i + 1, newPlaylists, pending)
                        (0, [], None)

                { state with Playlists = newPlaylists |> List.rev }, Cmd.none, [ NotifyPlaylistsChanged ]
        | None -> notifyError $"{nameof (OnMovePlaylist)} {nameof (Left)}: {nameof (Playlist)} {playlistId} not found"
    | OnMovePlaylist (playlistId, Right) ->
        match state.Playlists |> List.tryFindIndex (fun playlist -> playlist.Id = playlistId) with
        | Some index ->
            if index = state.Playlists.Length - 1 then
                notifyError
                    $"{nameof (OnMovePlaylist)} {nameof (Right)}: {nameof (Playlist)} {playlistId} already right-most"
            else
                let (_, newPlaylists, _) =
                    state.Playlists
                    |> List.fold
                        (fun (i, newPlaylists, pending) playlist ->
                            let newPlaylists, pending =
                                match pending with
                                | Some pending -> pending :: playlist :: newPlaylists, None
                                | None when i = index -> newPlaylists, Some playlist
                                | None -> playlist :: newPlaylists, None

                            i + 1, newPlaylists, pending)
                        (0, [], None)

                { state with Playlists = newPlaylists |> List.rev }, Cmd.none, [ NotifyPlaylistsChanged ]
        | None -> notifyError $"{nameof (OnMovePlaylist)} {nameof (Right)}: {nameof (Playlist)} {playlistId} not found"
    | OnMovePlaylist (playlistId, Up) ->
        notifyError $"{nameof (OnMovePlaylist)} ({playlistId}): {nameof (Up)} not supported"
    | OnMovePlaylist (playlistId, Down) ->
        notifyError $"{nameof (OnMovePlaylist)} ({playlistId}): {nameof (Down)} not supported"
    | OnRemovePlaylist playlistId ->
        let playlist, nextOrPrevious =
            state.Playlists
            |> List.rev
            |> List.fold
                (fun (playlist, nextOrPrevious) otherPlaylist ->
                    if otherPlaylist.Id = playlistId then
                        Some otherPlaylist, nextOrPrevious
                    else
                        match playlist, nextOrPrevious with
                        | None, _ -> None, Some otherPlaylist
                        | Some _, None -> playlist, Some otherPlaylist
                        | Some _, Some _ -> playlist, nextOrPrevious)
                (None, None)

        match playlist with
        | Some playlist ->
            let newPlaylists =
                state.Playlists
                |> List.filter (fun otherPlaylist -> otherPlaylist.Id <> playlist.Id)

            let hasCurrentTrack, isPlaying =
                match state.TrackState with
                | Some trackState ->
                    if
                        tracks playlist
                        |> List.exists (fun otherTrackData -> otherTrackData.Id = trackState.Track.Id)
                    then
                        match trackState.PlayerState with
                        | Playing _ -> true, true
                        | _ -> true, false
                    else
                        false, false
                | None -> false, false

            let selectedPlaylistId = nextOrPrevious |> Option.map (fun playlist -> playlist.Id)

            let cmd =
                match hasCurrentTrack, nextOrPrevious with
                | true, Some nextOrPrevious ->
                    Cmd.batch
                        [ Cmd.ofMsg RequestNoTrack
                          Cmd.ofMsg (RequestDefaultTrack(Some nextOrPrevious, isPlaying)) ]
                | true, None -> Cmd.ofMsg RequestNoTrack
                | _ -> Cmd.none

            { state with
                Playlists = newPlaylists
                SelectedPlaylistId = selectedPlaylistId },
            cmd,
            [ NotifyPlaylistsChanged ]
        | None -> notifyError $"{nameof (OnRemovePlaylist)}: {nameof (Playlist)} {playlistId} not found"
    | OnMoveTrack (trackId, direction) ->
        // TODO-NMB: For Up | Down, update Has[Previous|Next] for TrackState (if necessary) - and squash consecutive Summary items? - and call WritePlaylist...
        // TODO-NMB: For Left | Right, add at bottom (along with Summary above?) and update Has[Previous|Next] for TrackState (if necessary) - and call WritePlaylist (for both affected Playlists)...
        noChange
    | OnAddSubTotal (trackId, relativePosition) ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, _) ->
            match relativePosition with
            | Above ->
                let items = playlist.Items |> List.rev

                match items |> List.head with
                | Track trackData when trackData.Id = trackId ->
                    let newItems =
                        Track trackData :: SubTotal(SubTotalId.Create()) :: (items |> List.tail)

                    match updatePlaylists state.Playlists { playlist with Items = newItems |> List.rev } with
                    | Ok playlists -> { state with Playlists = playlists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
                    | Error error -> notifyError $"{nameof (OnAddSubTotal)} {nameof (Below)}: {error}"
                | _ ->
                    notifyError
                        $"{nameof (OnAddSubTotal)} {nameof (Above)}: {nameof (Track)} {trackId} is not the last item in the {nameof (Playlist)}"
            | Below ->
                match playlist.Items |> List.head with
                | Track trackData when trackData.Id = trackId ->
                    let newItems =
                        Track trackData
                        :: SubTotal(SubTotalId.Create()) :: (playlist.Items |> List.tail)

                    match updatePlaylists state.Playlists { playlist with Items = newItems } with
                    | Ok playlists -> { state with Playlists = playlists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
                    | Error error -> notifyError $"{nameof (OnAddSubTotal)} {nameof (Below)}: {error}"
                | _ ->
                    notifyError
                        $"{nameof (OnAddSubTotal)} {nameof (Below)}: {nameof (Track)} {trackId} is not the first item in the {nameof (Playlist)}"
        | Error error -> notifyError $"{nameof (OnAddSubTotal)}: {error}"
    | OnRemoveTrack trackId ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) ->
            match previousAndNext playlist trackData.Id with
            | Ok (previous, next) ->
                let isCurrentTrack, isPlaying =
                    match state.TrackState with
                    | Some trackState ->
                        if trackState.Track.Id = trackData.Id then
                            match trackState.PlayerState with
                            | Playing _ -> true, true
                            | _ -> true, false
                        else
                            false, false
                    | None -> false, false

                let newItems =
                    playlist.Items
                    |> List.filter (fun item ->
                        match item with
                        | Track otherTrackData when otherTrackData.Id = trackData.Id -> false
                        | _ -> true)
                    |> sanitize

                let writePlaylistCmd = Cmd.ofMsg (WritePlaylist playlist.Id)

                match updatePlaylists state.Playlists { playlist with Items = newItems } with
                | Ok playlists ->
                    let cmd =
                        match isCurrentTrack, previous, next with
                        | true, _, Some next ->
                            Cmd.batch
                                [ Cmd.ofMsg RequestNoTrack
                                  writePlaylistCmd
                                  Cmd.ofMsg (RequestTrack(next, playlist, isPlaying)) ]
                        | true, Some previous, _ ->
                            Cmd.batch
                                [ Cmd.ofMsg RequestNoTrack
                                  writePlaylistCmd
                                  Cmd.ofMsg (RequestTrack(previous, playlist, isPlaying)) ]
                        | true, None, None -> Cmd.batch [ Cmd.ofMsg RequestNoTrack; writePlaylistCmd ]
                        | _ -> writePlaylistCmd

                    { state with Playlists = playlists }, cmd, []
                | Error error -> notifyError $"{nameof (OnRemoveTrack)}: {error}"
            | Error error -> notifyError $"{nameof (OnRemoveTrack)}: {error}"
        | Error error -> notifyError $"{nameof (OnRemoveTrack)}: {error}"
    | OnRemoveSubTotal subTotalId ->
        match findSubTotal state.Playlists subTotalId with
        | Ok playlist ->
            let newItems =
                playlist.Items
                |> List.filter (fun item ->
                    match item with
                    | SubTotal otherSubTotalId when otherSubTotalId = subTotalId -> false
                    | _ -> true)

            match updatePlaylists state.Playlists { playlist with Items = newItems } with
            | Ok playlists -> { state with Playlists = playlists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
            | Error error -> notifyError $"{nameof (OnRemoveSubTotal)}: {error}"
        | Error error -> notifyError $"{nameof (OnRemoveSubTotal)}: {error}"
    | OnPlayTrack trackId ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) -> state, Cmd.ofMsg (RequestTrack(trackData, playlist, true)), []
        | Error error -> notifyError $"{nameof (OnPlayTrack)}: {error}"
    | OnSeek position ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing (_, lastPositionChanged) ->
                let changed =
                    match lastPositionChanged with
                    | Some lastPositionChanged ->
                        // Avoid glitches due to rounding errors (e.g. NotifyPositionChanged causes Slider to call RequestSeek).
                        let multiplier = 10000f

                        (Math.Round((position * multiplier) |> double) |> int)
                        <> (Math.Round((lastPositionChanged * multiplier) |> double) |> int)
                    | None -> true

                if changed then
                    let seekRequest = SeekRequestId.Create(), position

                    let delay () =
                        async {
                            do! Async.Sleep DEBOUNCE_SEEK_REQUEST_DELAY
                            return seekRequest
                        }

                    { state with
                        TrackState = Some { trackState with PlayerState = Playing(position, Some position) }
                        SeekRequests = (fst seekRequest) :: state.SeekRequests },
                    Cmd.OfAsync.perform delay () DebounceSeekRequest,
                    []
                else
                    noChange
            | Paused _ ->
                { state with TrackState = Some { trackState with PlayerState = Paused position } }, Cmd.none, []
            | Stopped _ ->
                { state with TrackState = Some { trackState with PlayerState = Stopped position } }, Cmd.none, []
            | _ -> noChange // TODO-NMB: No need to notify error for this?...notifyError $"{nameof (Seek)} when {nameof (PlayerState)} is not {nameof (Playing)}, {nameof (Paused)} or {nameof (Stopped)}"
        | None -> noChange // TODO-NMB: No need to notify error for this?...notifyError $"{nameof (Seek)} when trackState is {nameof (None)}"
    | OnPrevious ->
        match state.TrackState with
        | Some trackState ->
            match trackState.Previous with
            | Some previous ->
                match findTrack state.Playlists previous.Id with
                | Ok (playlist, trackData) ->
                    let play =
                        match trackState.PlayerState with
                        | Playing _ -> true
                        | _ -> false

                    state, Cmd.ofMsg (RequestTrack(previous, playlist, play)), []
                | Error error -> notifyError $"{nameof (OnPrevious)}: {error}"
            | None -> notifyError $"{nameof (OnPrevious)} when {nameof (TrackState)}.Previous is None"
        | None -> notifyError $"{nameof (OnPrevious)} when {nameof (TrackState)} is {nameof (None)}"
    | OnNext ->
        match state.TrackState with
        | Some trackState ->
            match trackState.Next with
            | Some next ->
                match findTrack state.Playlists next.Id with
                | Ok (playlist, trackData) ->
                    let play =
                        match trackState.PlayerState with
                        | Playing _
                        | Ended -> true
                        | _ -> false

                    state, Cmd.ofMsg (RequestTrack(next, playlist, play)), []
                | Error error -> notifyError $"{nameof (OnNext)}: {error}"
            | None -> notifyError $"{nameof (OnNext)} when {nameof (TrackState)}.Next is None"
        | None -> notifyError $"{nameof (OnNext)} when {nameof (TrackState)} is {nameof (None)}"
    | OnPlay ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | NoMedia
            | Ended ->
                playTrack trackState.Track player

                { state with TrackState = Some { trackState with PlayerState = AwaitingPlay } },
                Cmd.none,
                [ NotifyTrackStateChanged ]
            | AwaitingPlay _ -> notifyError $"{nameof (OnPlay)} when {nameof (PlayerState)} is {nameof (AwaitingPlay)}"
            | Playing _ -> notifyError $"{nameof (OnPlay)} when {nameof (PlayerState)} already {nameof (Playing)}"
            | Paused position ->
                player.Pause()
                player.Position <- position

                { state with TrackState = Some { trackState with PlayerState = Playing(position, None) } },
                Cmd.none,
                [ NotifyTrackStateChanged ]
            | Stopped position ->
                player.Play() |> ignore
                player.Position <- position

                { state with TrackState = Some { trackState with PlayerState = Playing(position, None) } },
                Cmd.none,
                [ NotifyTrackStateChanged ]
            | PlaybackErrored ->
                notifyError $"{nameof (OnPlay)} when {nameof (PlayerState)} is {nameof (PlaybackErrored)}"
        | None -> notifyError $"{nameof (OnPlay)} when {nameof (TrackState)} is {nameof (None)}"
    | OnPause ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing (position, _) ->
                player.Pause()

                { state with TrackState = Some { trackState with PlayerState = Paused position } },
                Cmd.none,
                [ NotifyTrackStateChanged ]
            | _ -> notifyError $"{nameof (OnPause)} when {nameof (PlayerState)} is not {nameof (Playing)}"
        | None -> notifyError $"{nameof (OnPause)} when {nameof (TrackState)} is {nameof (None)}"
    | OnStop ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _
            | Paused _ ->
                player.Stop()

                { state with TrackState = Some { trackState with PlayerState = Stopped START_POSITION } },
                Cmd.none,
                [ NotifyTrackStateChanged ]
            | _ ->
                notifyError
                    $"{nameof (OnStop)} when {nameof (PlayerState)} is not {nameof (Playing)} or {nameof (Paused)}"
        | None -> notifyError $"{nameof (OnStop)} when {nameof (TrackState)} is {nameof (None)}"
    | OnToggleMuted ->
        let newMuted = not state.Muted
        player.Mute <- newMuted
        { state with Muted = newMuted }, Cmd.none, [ NotifyMutedToggled ]
    | OnVolume volume ->
        if volume <> state.Volume then
            let newMuted = volume = 0
            player.Mute <- newMuted
            player.Volume <- playerVolume volume

            let externalMsgs =
                [ NotifyVolumeChanged
                  if newMuted <> state.Muted then
                      NotifyMutedToggled ]

            { state with
                Muted = newMuted
                Volume = volume },
            Cmd.none,
            externalMsgs
        else
            noChange
    // From MediaPlayer subscriptions
    | NotifyErrored ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | AwaitingPlay _ ->
                { state with TrackState = Some { trackState with PlayerState = PlaybackErrored } },
                Cmd.none,
                [ NotifyTrackStateChanged ]
            | _ -> notifyError $"{nameof (NotifyErrored)} when {nameof (PlayerState)} is not {nameof (AwaitingPlay)}"
        | None -> notifyError $"{nameof (NotifyErrored)} when {nameof (TrackState)} is {nameof (None)}"
    | NotifyPlaying ->
        match state.TrackState with
        | Some trackState ->
            let duration = player.Length * 1L<millisecond>

            let newTrackState, externalMsgs =
                match trackState.PlayerState with
                | AwaitingPlay _ ->
                    { trackState with
                        Track = { trackState.Track with Duration = Some duration }
                        PlayerState = Playing(START_POSITION, None) },
                    [ NotifyTrackStateChanged ]
                | _ -> trackState, []

            if trackState.Track.Duration <> Some duration then
                match findTrack state.Playlists trackState.Track.Id with
                | Ok (playlist, trackData) ->
                    match updatePlaylist playlist { trackData with Duration = Some duration } with
                    | Ok newPlaylist ->
                        match updatePlaylists state.Playlists newPlaylist with
                        | Ok newPlaylists ->
                            { state with
                                Playlists = newPlaylists
                                TrackState = Some newTrackState },
                            Cmd.ofMsg (WritePlaylist newPlaylist.Id),
                            externalMsgs
                        | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}"
                    | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}"
                | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}"
            else
                { state with TrackState = Some newTrackState }, Cmd.none, externalMsgs
        | None -> notifyError $"{nameof (NotifyPlaying)} when {nameof (TrackState)} is {nameof (None)}"
    | NotifyPositionChanged position ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing (currentPosition, _) ->
                let newPosition =
                    match state.SeekRequests with
                    | _ :: _ -> currentPosition
                    | [] -> position

                { state with TrackState = Some { trackState with PlayerState = Playing(newPosition, Some position) } },
                Cmd.none,
                []
            | _ -> notifyError $"{nameof (NotifyPositionChanged)} when {nameof (PlayerState)} not {nameof (Playing)}"
        | None -> notifyError $"{nameof (NotifyPositionChanged)} when {nameof (TrackState)} is {nameof (None)}"
    | NotifyEnded ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _ ->
                let cmd =
                    if trackState.Next |> Option.isSome then
                        Cmd.ofMsg OnNext
                    else
                        Cmd.none

                { state with TrackState = Some { trackState with PlayerState = Ended } },
                cmd,
                [ NotifyTrackStateChanged ]
            | _ -> notifyError $"{nameof (NotifyEnded)} when {nameof (PlayerState)} not {nameof (Playing)}"
        | None -> notifyError $"{nameof (NotifyEnded)} when {nameof (TrackState)} is {nameof (None)}"
