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
    | RequestTrack of trackData: TrackData * hasPrevious: bool * hasNext: bool * play: bool
    | RequestNoTrack
    // UI
    | OnSelectPlaylist of PlaylistId
    | OnMovePlaylist of PlaylistId * Direction
    | OnRemovePlaylist of PlaylistId
    | OnMoveTrack of TrackId * Direction
    | OnAddSummary of TrackId * RelativePosition
    | OnRemoveTrack of TrackId
    | OnRemoveSummary of SummaryId
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

let private playlist playlists playlistId =
    playlists |> List.tryFind (fun otherPlaylist -> otherPlaylist.Id = playlistId)

let isTrackId trackId =
    function
    | Track track when track.Id = trackId -> true
    | _ -> false

let private findSummary playlists summaryId =
    let matches =
        playlists
        |> List.choose (fun playlist ->
            let summaryMatches =
                playlist.Items
                |> List.choose (fun item ->
                    match item with
                    | Summary otherSummaryId when otherSummaryId = Some summaryId -> Some item
                    | _ -> None)

            match summaryMatches with
            | _ :: _ -> Some(playlist, summaryMatches)
            | [] -> None)

    match matches with
    | [ playlist, summaryMatches ] ->
        match summaryMatches with
        | [ _ ] -> Ok playlist
        | [] -> Error $"no matches for {summaryId} for {nameof (Playlist)} {playlist.Name}"
        | _ -> Error $"multiple matches for {summaryId} for {nameof (Playlist)} {playlist.Name}"
    | [] -> Error $"no matches for {summaryId}"
    | _ -> Error $"matches for {summaryId} for multiple {nameof (Playlist)}s"

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

let private hasPreviousAndNext playlist trackId =
    match previousAndNext playlist trackId with
    | Ok (Some _, Some _) -> Ok(true, true)
    | Ok (Some _, None) -> Ok(true, false)
    | Ok (None, Some _) -> Ok(false, true)
    | Ok (None, None) -> Ok(false, false)
    | Error error -> Error error

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

let private playTrack track (player: MediaPlayer) =
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
    ReadPlaylists(playlistIds, lastTrackId, autoPlay, [])

(* Notes:
    -- If change TrackState.Track (including setting TrackState to None) or TrackState.PlayerStatus, call (external) NotifyTrackStateChanged.
    -- If add / remove / reorder Playlists, call (external) NotifyPlaylistsChanged.
    -- If add new Playlist, call (external) NewPlaylistAdded?
    -- If add / remove / reorder Playlist items, call (internal) WritePlaylist. *)

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
                        match hasPreviousAndNext playlist trackData.Id with
                        | Ok (hasPrevious, hasNext) ->
                            Some(playlist.Id, Cmd.ofMsg (RequestTrack(trackData, hasPrevious, hasNext, autoPlay))), []
                        | Error error -> None, [ NotifyError $"Playlists.transition -> {error}" ]
                    | Error error -> None, [ NotifyError $"Playlists.transition -> {error}" ]
                | None -> None, []

            let selectedPlaylistId, cmd, externalMsgs =
                match lastTrack with
                | (Some selectedPlaylistIdAndMsg, externalMsgs) ->
                    Some(fst selectedPlaylistIdAndMsg), snd selectedPlaylistIdAndMsg, externalMsgs
                | (None, externalMsgs) ->
                    let playlistsWithTracks =
                        playlists
                        |> List.choose (fun playlist ->
                            match tracks playlist with
                            | trackData :: _ -> Some(playlist, trackData)
                            | [] -> None)

                    match playlistsWithTracks with
                    | (playlist, trackData) :: _ ->
                        match hasPreviousAndNext playlist trackData.Id with
                        | Ok (hasPrevious, hasNext) ->
                            Some playlist.Id,
                            Cmd.ofMsg (RequestTrack(trackData, hasPrevious, hasNext, autoPlay)),
                            externalMsgs
                        | Error error ->
                            None,
                            Cmd.ofMsg RequestNoTrack,
                            NotifyError $"Playlists.transition -> {error}" :: externalMsgs
                    | [] -> None, Cmd.ofMsg RequestNoTrack, externalMsgs

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
    | RequestTrack (trackData, hasPrevious, hasNext, play) ->
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
                                      HasPrevious = hasPrevious
                                      HasNext = hasNext } },
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
                                  HasPrevious = hasPrevious
                                  HasNext = hasNext } },
                    Cmd.none,
                    [ NotifyTrackStateChanged ]
                )

        match newStateAndCmdAndExternalMsgs with
        | Ok (newState, cmd, externalMsgs) -> newState, cmd, externalMsgs
        | Error error -> notifyError error
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
    | OnMovePlaylist (playlistId, direction) -> // TODO-NMB: Not working properly - perhaps because of caching weirdness with tabs?...
        match direction with
        | Left ->
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
                                    | Some pending -> playlist :: pending :: newPlaylists, None
                                    | None when i = index - 1 -> newPlaylists, Some playlist
                                    | None -> playlist :: newPlaylists, None

                                i + 1, newPlaylists, pending)
                            (0, [], None)

                    { state with Playlists = newPlaylists }, Cmd.none, [ NotifyPlaylistsChanged ]
            | None ->
                notifyError $"{nameof (OnMovePlaylist)} {nameof (Left)}: {nameof (Playlist)} {playlistId} not found"
        | Right ->
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
                                    | Some pending -> playlist :: pending :: newPlaylists, None
                                    | None when i = index -> newPlaylists, Some playlist
                                    | None -> playlist :: newPlaylists, None

                                i + 1, newPlaylists, pending)
                            (0, [], None)

                    { state with Playlists = newPlaylists }, Cmd.none, [ NotifyPlaylistsChanged ]
            | None ->
                notifyError $"{nameof (OnMovePlaylist)} {nameof (Right)}: {nameof (Playlist)} {playlistId} not found"
        | _ -> notifyError $"{nameof (OnMovePlaylist)} ({playlistId}) when {nameof (Direction)} is {direction}"
    | OnRemovePlaylist playlistId ->
        // TODO-NMB: Handle case where TrackState is for removed Playlist - and call (external) NotifyPlaylistsChanged...
        noChange
    | OnMoveTrack (trackId, direction) ->
        // TODO-NMB: For Up | Down, update Has[Previous|Next] for TrackState (if necessary) - and squash consecutive Summary items? - and call WritePlaylist...
        // TODO-NMB: For Left | Right, add at bottom (along with Summary above?) and update Has[Previous|Next] for TrackState (if necessary) - and call WritePlaylist (for both affected Playlists)...
        noChange
    | OnAddSummary (trackId, relativePosition) -> // TODO-NMB: Squash consecutive Summary items (albeit should never happen)?...
        match findTrack state.Playlists trackId with
        | Ok (playlist, _) ->
            match relativePosition with
            | Above ->
                let items = playlist.Items |> List.rev

                match items |> List.head with
                | Track trackData when trackData.Id = trackId ->
                    let newItems =
                        Track trackData :: Summary(Some(SummaryId.Create())) :: (items |> List.tail)

                    match updatePlaylists state.Playlists { playlist with Items = newItems |> List.rev } with
                    | Ok playlists -> { state with Playlists = playlists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
                    | Error error -> notifyError $"{nameof (OnAddSummary)} {nameof (Below)}: {error}"
                | _ ->
                    notifyError
                        $"{nameof (OnAddSummary)} {nameof (Above)}: {nameof (Track)} {trackId} is not the last item in the {nameof (Playlist)}"
            | Below ->
                match playlist.Items |> List.head with
                | Track trackData when trackData.Id = trackId ->
                    let newItems =
                        Track trackData
                        :: Summary(Some(SummaryId.Create())) :: (playlist.Items |> List.tail)

                    match updatePlaylists state.Playlists { playlist with Items = newItems } with
                    | Ok playlists -> { state with Playlists = playlists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
                    | Error error -> notifyError $"{nameof (OnAddSummary)} {nameof (Below)}: {error}"
                | _ ->
                    notifyError
                        $"{nameof (OnAddSummary)} {nameof (Below)}: {nameof (Track)} {trackId} is not the first item in the {nameof (Playlist)}"
        | Error error -> notifyError $"{nameof (OnAddSummary)}: {error}"
    | OnRemoveTrack trackId ->
        // TODO-NMB: Handle case where TrackState is for removed Track - and call WritePlaylist...
        noChange
    | OnRemoveSummary summaryId ->
        match findSummary state.Playlists summaryId with
        | Ok playlist ->
            let newItems =
                playlist.Items
                |> List.filter (fun item ->
                    match item with
                    | Summary otherSummaryId when otherSummaryId = Some summaryId -> false
                    | _ -> true)

            match updatePlaylists state.Playlists { playlist with Items = newItems } with
            | Ok playlists -> { state with Playlists = playlists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
            | Error error -> notifyError $"{nameof (OnRemoveSummary)}: {error}"
        | Error error -> notifyError $"{nameof (OnRemoveSummary)}: {error}"
    | OnPlayTrack trackId ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) ->
            match hasPreviousAndNext playlist trackData.Id with
            | Ok (hasPrevious, hasNext) -> state, Cmd.ofMsg (RequestTrack(trackData, hasPrevious, hasNext, true)), []
            | Error error -> notifyError $"{nameof (OnPlayTrack)}: {error}"
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
        | Some trackState when trackState.HasPrevious ->
            match findTrack state.Playlists trackState.Track.Id with
            | Ok (playlist, trackData) ->
                match previousAndNext playlist trackData.Id with
                | Ok (Some previous, _) ->
                    match hasPreviousAndNext playlist previous.Id with
                    | Ok (hasPrevious, hasNext) ->
                        let play =
                            match trackState.PlayerState with
                            | Playing _ -> true
                            | _ -> false

                        state, Cmd.ofMsg (RequestTrack(previous, hasPrevious, hasNext, play)), []
                    | Error error -> notifyError $"{nameof (OnPrevious)}: {error}"
                | Ok (None, _) ->
                    notifyError
                        $"{nameof (OnPrevious)}: no previous track for {trackData.Id} for {nameof (Playlist)} {playlist.Name}"
                | Error error -> notifyError $"{nameof (OnPrevious)}: {error}"
            | Error error -> notifyError $"{nameof (OnPrevious)}: {error}"
        | Some _ -> notifyError $"{nameof (OnPrevious)} when not {nameof (TrackState)}.HasPrevious"
        | None -> notifyError $"{nameof (OnPrevious)} when {nameof (TrackState)} is {nameof (None)}"
    | OnNext ->
        match state.TrackState with
        | Some trackState when trackState.HasNext ->
            match findTrack state.Playlists trackState.Track.Id with
            | Ok (playlist, trackData) ->
                match previousAndNext playlist trackData.Id with
                | Ok (_, Some next) ->
                    match hasPreviousAndNext playlist next.Id with
                    | Ok (hasPrevious, hasNext) ->
                        let play =
                            match trackState.PlayerState with
                            | Playing _
                            | Ended -> true
                            | _ -> false

                        state, Cmd.ofMsg (RequestTrack(next, hasPrevious, hasNext, play)), []
                    | Error error -> notifyError $"{nameof (OnNext)}: {error}"
                | Ok (_, None) ->
                    notifyError
                        $"{nameof (OnNext)}: no next track for {trackData.Id} for {nameof (Playlist)} {playlist.Name}"
                | Error error -> notifyError $"{nameof (OnNext)}: {error}"
            | Error error -> notifyError $"{nameof (OnNext)}: {error}"
        | Some _ -> notifyError $"{nameof (OnNext)} when not {nameof (TrackState)}.HasPrevious"
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
                let cmd = if trackState.HasNext then Cmd.ofMsg OnNext else Cmd.none

                { state with TrackState = Some { trackState with PlayerState = Ended } },
                cmd,
                [ NotifyTrackStateChanged ]
            | _ -> notifyError $"{nameof (NotifyEnded)} when {nameof (PlayerState)} not {nameof (Playing)}"
        | None -> notifyError $"{nameof (NotifyEnded)} when {nameof (TrackState)} is {nameof (None)}"
