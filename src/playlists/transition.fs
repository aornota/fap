module Aornota.Fap.Playlists.Transition

open Aornota.Fap.Playlists.Model
open Aornota.Fap.Persistence
open Aornota.Fap.Utilities
open Elmish
open LibVLCSharp.Shared
open System
open System.IO

type HorizontalDirection =
    | Left
    | Right

type VerticalDirection =
    | Up
    | Down

type Direction =
    | Horizontal of HorizontalDirection
    | Vertical of VerticalDirection

type RelativePosition =
    | Above
    | Below

type ExternalMsg =
    | NotifyNewPlaylistAdded of Playlist
    | NotifyTracksAddedOrRemoved of Playlist
    | NotifyPlaylistsChanged
    | NotifyTrackStateChanged
    | NotifyMutedToggled
    | NotifyVolumeChanged
    | NotifyError of string * string option

type Msg =
    // Internal
    | NoOp
    | ReadPlaylists of PlaylistId list * TrackId option * bool * Playlist list
    | ReadPlaylistError of ReadError * PlaylistId * PlaylistId list * TrackId option * bool * Playlist list
    | WritePlaylist of PlaylistId
    | DebounceWritePlaylistRequest of WritePlaylistRequestId * PlaylistId
    | WritePlaylistError of string * PlaylistId
    | DebounceSeekRequest of SeekRequestId * float32
    | RequestDefaultTrack of Playlist option * bool
    | RequestTrack of TrackData * Playlist * bool
    | RequestNoTrack
    | AddToPlaylist of TrackData list
    | UpdatePreviousAndNext of Playlist
    // UI
    | OnSelectPlaylist of PlaylistId
    | OnMovePlaylist of PlaylistId * HorizontalDirection
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
    // From App
    | NotifyNewPlaylistRequested
    | NotifyPlaylistOpened of Playlist
    | NotifyFilesAdded of string list
    | NotifyFolderAdded of string
    // From MediaPlayer subscriptions
    | NotifyErrored
    | NotifyPlaying
    | NotifyPositionChanged of float32
    | NotifyEnded

[<Literal>]
let private ERROR_PREFIX = "Playlists -> "

[<Literal>]
let START_POSITION = 0f

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

let transition msg state (player: MediaPlayer) =
    let notifyError error nonDebugMessage =
        state, Cmd.none, [ NotifyError($"{ERROR_PREFIX}{error}", nonDebugMessage) ]

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
                    | Error error -> None, [ NotifyError($"{ERROR_PREFIX}{nameof (ReadPlaylists)}: {error}", None) ]
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
    | ReadPlaylistError (readError, playlistId, playlistIds, lastTrackId, autoPlay, playlists) ->
        let (PlaylistId guid) = playlistId

        state,
        Cmd.ofMsg (ReadPlaylists(playlistIds, lastTrackId, autoPlay, playlists)),
        [ NotifyError(
              $"{ERROR_PREFIX}{nameof (ReadPlaylistError)} ({playlistId}): {readErrorText readError}",
              Some $"Unable to read playlist ({guid})"
          ) ]
    | WritePlaylist playlistId ->
        let writePlaylistRequest = WritePlaylistRequestId.Create(), playlistId

        let delay () =
            async {
                do! Async.Sleep 500
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
                    match
                        state.Playlists
                        |> List.tryFind (fun otherPlaylist -> otherPlaylist.Id = playlistId)
                    with
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
    | WritePlaylistError (error, playlistId) ->
        let (PlaylistId guid) = playlistId

        notifyError $"{nameof (WritePlaylistError)} ({playlistId}): {error}" (Some $"Unable to write playlist ({guid})")
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
            | _ -> notifyError $"{nameof (DebounceSeekRequest)}: {nameof (PlayerState)} not {nameof (Playing)}" None
        | None -> notifyError $"{nameof (DebounceSeekRequest)}: {nameof (TrackState)} is {nameof (None)}" None
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
        | None ->
            match state.Playlists with
            | playlist :: _ -> { state with SelectedPlaylistId = Some playlist.Id }, Cmd.none, []
            | [] -> noChange
    | RequestTrack (trackData, playlist, play) ->
        match previousAndNext playlist trackData.Id with
        | Ok (previous, next) ->
            let newStateAndCmdAndExternalMsgs =
                match state.TrackState with
                | Some trackState ->
                    if trackState.Track.Id = trackData.Id then
                        if not play then
                            Error
                                $"{nameof (RequestTrack)}: {nameof (TrackState)} is already requested {nameof (TrackData)} and not play"
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
            | Error error -> notifyError error None
        | Error error -> notifyError $"{nameof (RequestTrack)}: {error}" None
    | RequestNoTrack ->
        player.Media <- null

        { state with
            TrackState = None
            SeekRequests = [] },
        Cmd.none,
        [ NotifyTrackStateChanged ]
    | AddToPlaylist trackDatas ->
        match state.SelectedPlaylistId with
        | Some playlistId ->
            match state.Playlists |> List.tryFind (fun playlist -> playlist.Id = playlistId) with
            | Some playlist ->
                let trackDatas = trackDatas |> List.sortBy (fun trackData -> trackData.Name)

                match trackDatas with
                | trackData :: _ ->
                    let requestTrack =
                        if state.Playlists.Length = 1 && tracks playlist |> List.length = 0 then
                            Some trackData
                        else
                            None

                    let items = SubTotal(SubTotalId.Create()) :: (trackDatas |> List.map Track)

                    let newPlaylist = { playlist with Items = (playlist.Items @ items) |> sanitize }

                    let writePlaylistCmd = Cmd.ofMsg (WritePlaylist playlist.Id)

                    match updatePlaylists state.Playlists newPlaylist with
                    | Ok playlists ->
                        let cmd =
                            match requestTrack with
                            | Some trackData ->
                                Cmd.batch [ Cmd.ofMsg (RequestTrack(trackData, newPlaylist, false)); writePlaylistCmd ]
                            | None -> Cmd.batch [ Cmd.ofMsg (UpdatePreviousAndNext newPlaylist); writePlaylistCmd ]

                        { state with Playlists = playlists }, cmd, [ NotifyTracksAddedOrRemoved newPlaylist ]
                    | Error error -> notifyError $"{nameof (AddToPlaylist)}: {error}" None
                | [] -> notifyError $"{nameof (AddToPlaylist)}: no {nameof (Track)}s to add" None
            | None -> notifyError $"{nameof (AddToPlaylist)}: ({playlistId}) not found" None
        | None -> notifyError $"{nameof (AddToPlaylist)} when no selected playlist" None
    | UpdatePreviousAndNext playlist ->
        match state.TrackState with
        | Some trackState ->
            if playlist.Items |> List.exists (fun item -> isTrackId trackState.Track.Id item) then
                match previousAndNext playlist trackState.Track.Id with
                | Ok (previous, next) ->
                    let newTrackState =
                        { trackState with
                            Previous = previous
                            Next = next }

                    { state with TrackState = Some newTrackState }, Cmd.none, []
                | Error error -> notifyError $"{nameof (UpdatePreviousAndNext)}: {error}" None
            else
                noChange
        | None -> noChange
    // From UI
    | OnSelectPlaylist playlistId -> { state with SelectedPlaylistId = Some playlistId }, Cmd.none, []
    | OnMovePlaylist (playlistId, horizontalDirection) ->
        match state.Playlists |> List.tryFindIndex (fun playlist -> playlist.Id = playlistId) with
        | Some index ->
            match horizontalDirection, index with
            | Left, 0 ->
                notifyError $"{nameof (OnMovePlaylist)} {horizontalDirection} ({playlistId}): already left-most" None
            | Right, index when index = state.Playlists.Length - 1 ->
                notifyError $"{nameof (OnMovePlaylist)} {horizontalDirection} ({playlistId}): already right-most" None
            | _ ->
                let (_, newPlaylists, _) =
                    let index =
                        match horizontalDirection with
                        | Left -> index - 1
                        | Right -> index

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
        | None -> notifyError $"{nameof (OnMovePlaylist)} {horizontalDirection} ({playlistId}): not found" None
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
        | None -> notifyError $"{nameof (OnRemovePlaylist)} ({playlistId}): not found" None
    | OnMoveTrack (trackId, Vertical verticalDirection) ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) ->
            match
                playlist.Items
                |> List.tryFindIndex (fun item ->
                    match item with
                    | Track otherTrackData when otherTrackData.Id = trackData.Id -> true
                    | _ -> false)
            with
            | Some index ->
                match verticalDirection, index with
                | Up, 0 ->
                    notifyError $"{nameof (OnMoveTrack)} {verticalDirection} ({trackData.Id}): already first" None
                | Down, index when index = playlist.Items.Length - 1 ->
                    notifyError $"{nameof (OnMoveTrack)} {verticalDirection} ({trackData.Id}): already last" None
                | _ ->
                    let index =
                        match verticalDirection with
                        | Up -> index - 1
                        | Down -> index

                    let (_, newItems, _) =
                        playlist.Items
                        |> List.fold
                            (fun (i, newItems, pending) item ->
                                let newItems, pending =
                                    match pending with
                                    | Some pending -> pending :: item :: newItems, None
                                    | None when i = index -> newItems, Some item
                                    | None -> item :: newItems, None

                                i + 1, newItems, pending)
                            (0, [], None)

                    let newPlaylist = { playlist with Items = newItems |> List.rev |> sanitize }

                    let trackStateForPlaylist =
                        match state.TrackState with
                        | Some trackState ->
                            if
                                tracks newPlaylist
                                |> List.exists (fun otherTrackData -> otherTrackData.Id = trackState.Track.Id)
                            then
                                Some trackState
                            else
                                None
                        | None -> None

                    let writePlaylistCmd = Cmd.ofMsg (WritePlaylist playlist.Id)

                    match updatePlaylists state.Playlists newPlaylist with
                    | Ok playlists ->
                        match trackStateForPlaylist with
                        | Some trackStateForPlaylist ->
                            match previousAndNext newPlaylist trackStateForPlaylist.Track.Id with
                            | Ok (previous, next) ->
                                let newTrackState =
                                    { trackStateForPlaylist with
                                        Previous = previous
                                        Next = next }

                                { state with
                                    Playlists = playlists
                                    TrackState = Some newTrackState },
                                writePlaylistCmd,
                                []
                            | Error error ->
                                notifyError $"{nameof (OnMoveTrack)} {verticalDirection} ({trackData.Id}): {error}" None
                        | None -> { state with Playlists = playlists }, writePlaylistCmd, []
                    | Error error ->
                        notifyError $"{nameof (OnMoveTrack)} {verticalDirection} ({trackData.Id}): {error}" None
            | _ -> notifyError $"{nameof (OnMoveTrack)} {verticalDirection} ({trackData.Id}): not found" None
        | Error error -> notifyError $"{nameof (OnMoveTrack)} {verticalDirection} ({trackId}): {error}" None
    | OnMoveTrack (trackId, Horizontal (horizontalDirection: HorizontalDirection)) ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) ->
            match
                state.Playlists
                |> List.tryFindIndex (fun otherPlaylist -> otherPlaylist.Id = playlist.Id)
            with
            | Some index ->
                match horizontalDirection, index with
                | Left, 0 ->
                    notifyError
                        $"{nameof (OnMoveTrack)} {horizontalDirection} ({trackData.Id}): already for left-most {nameof (Playlist)}"
                        None
                | Right, index when index = state.Playlists.Length - 1 ->
                    notifyError
                        $"{nameof (OnMoveTrack)} {horizontalDirection} ({trackData.Id}): already for right-most {nameof (Playlist)}"
                        None
                | _ ->
                    let otherPlaylist =
                        match horizontalDirection with
                        | Left -> state.Playlists[index - 1]
                        | Right -> state.Playlists[index + 1]

                    let newItems =
                        playlist.Items
                        |> List.filter (fun item -> not (isTrackId trackData.Id item))
                        |> sanitize

                    let newOtherItems =
                        Track trackData
                        :: SubTotal(SubTotalId.Create()) :: (otherPlaylist.Items |> List.rev)
                        |> List.rev
                        |> sanitize

                    let newPlaylist, newOtherPlaylist =
                        { playlist with Items = newItems }, { otherPlaylist with Items = newOtherItems }

                    let trackStateForPlaylist =
                        match state.TrackState with
                        | Some trackState ->
                            if
                                tracks newPlaylist
                                |> List.exists (fun otherTrackData -> otherTrackData.Id = trackState.Track.Id)
                            then
                                Some(trackState, newPlaylist)
                            else if
                                tracks newOtherPlaylist
                                |> List.exists (fun otherTrackData -> otherTrackData.Id = trackState.Track.Id)
                            then
                                Some(trackState, newOtherPlaylist)
                            else
                                None
                        | None -> None

                    let writePlaylistCmd = Cmd.ofMsg (WritePlaylist playlist.Id)

                    match updatePlaylists state.Playlists newPlaylist with
                    | Ok playlists ->
                        match updatePlaylists playlists newOtherPlaylist with
                        | Ok playlists ->
                            match trackStateForPlaylist with
                            | Some (trackStateForPlaylist, playlist) ->
                                match previousAndNext playlist trackStateForPlaylist.Track.Id with
                                | Ok (previous, next) ->
                                    let newTrackState =
                                        { trackStateForPlaylist with
                                            Previous = previous
                                            Next = next }

                                    { state with
                                        Playlists = playlists
                                        TrackState = Some newTrackState },
                                    writePlaylistCmd,
                                    []
                                | Error error ->
                                    notifyError $"{nameof (OnMoveTrack)} {horizontalDirection}: {error}" None
                            | None -> { state with Playlists = playlists }, writePlaylistCmd, []
                        | Error error ->
                            notifyError $"{nameof (OnMoveTrack)} ({trackData.Id}): {horizontalDirection} {error}" None
                    | Error error ->
                        notifyError $"{nameof (OnMoveTrack)} ({trackData.Id}): {horizontalDirection} {error}" None
            | None ->
                notifyError
                    $"{nameof (OnMoveTrack)} {horizontalDirection} ({trackData.Id}): {nameof (Playlist)} not found"
                    None
        | Error error -> notifyError $"{nameof (OnMoveTrack)} {horizontalDirection} ({trackId}): {error}" None
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
                    | Error error -> notifyError $"{nameof (OnAddSubTotal)} {nameof (Below)} ({trackId}): {error}" None
                | _ ->
                    notifyError
                        $"{nameof (OnAddSubTotal)} {nameof (Above)} ({trackId}): not the last item in the {nameof (Playlist)}"
                        None
            | Below ->
                match playlist.Items |> List.head with
                | Track trackData when trackData.Id = trackId ->
                    let newItems =
                        Track trackData
                        :: SubTotal(SubTotalId.Create()) :: (playlist.Items |> List.tail)

                    match updatePlaylists state.Playlists { playlist with Items = newItems } with
                    | Ok playlists -> { state with Playlists = playlists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
                    | Error error -> notifyError $"{nameof (OnAddSubTotal)} {nameof (Below)} ({trackId}): {error}" None
                | _ ->
                    notifyError
                        $"{nameof (OnAddSubTotal)} {nameof (Below)} ({trackId}): not the first item in the {nameof (Playlist)}"
                        None
        | Error error -> notifyError $"{nameof (OnAddSubTotal)}: {error}" None
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
                    playlist.Items |> List.filter (fun item -> not (isTrackId trackData.Id item))

                let newPlaylist = { playlist with Items = newItems |> sanitize }

                let writePlaylistCmd = Cmd.ofMsg (WritePlaylist playlist.Id)

                match updatePlaylists state.Playlists newPlaylist with
                | Ok playlists ->
                    let cmd =
                        match isCurrentTrack, previous, next with
                        | true, _, Some next ->
                            Cmd.batch
                                [ Cmd.ofMsg RequestNoTrack
                                  writePlaylistCmd
                                  Cmd.ofMsg (RequestTrack(next, newPlaylist, isPlaying)) ]
                        | true, Some previous, _ ->
                            Cmd.batch
                                [ Cmd.ofMsg RequestNoTrack
                                  writePlaylistCmd
                                  Cmd.ofMsg (RequestTrack(previous, newPlaylist, isPlaying)) ]
                        | true, None, None -> Cmd.batch [ Cmd.ofMsg RequestNoTrack; writePlaylistCmd ]
                        | _ -> Cmd.batch [ Cmd.ofMsg (UpdatePreviousAndNext newPlaylist); writePlaylistCmd ]

                    { state with Playlists = playlists }, cmd, [ NotifyTracksAddedOrRemoved newPlaylist ]
                | Error error -> notifyError $"{nameof (OnRemoveTrack)} ({trackId}): {error}" None
            | Error error -> notifyError $"{nameof (OnRemoveTrack)} ({trackId}): {error}" None
        | Error error -> notifyError $"{nameof (OnRemoveTrack)} ({trackId}): {error}" None
    | OnRemoveSubTotal subTotalId ->
        let matches =
            state.Playlists
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
            | [ _ ] ->
                let newItems =
                    playlist.Items
                    |> List.filter (fun item ->
                        match item with
                        | SubTotal otherSubTotalId when otherSubTotalId = subTotalId -> false
                        | _ -> true)

                match updatePlaylists state.Playlists { playlist with Items = newItems } with
                | Ok playlists -> { state with Playlists = playlists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
                | Error error -> notifyError $"{nameof (OnRemoveSubTotal)} ({subTotalId}): {error}" None
            | [] ->
                notifyError
                    $"{nameof (OnRemoveSubTotal)} ({subTotalId}): no matches for {subTotalId} for {nameof (Playlist)} {playlist.Name}"
                    None
            | _ ->
                notifyError
                    $"{nameof (OnRemoveSubTotal)}: multiple matches for {subTotalId} for {nameof (Playlist)} {playlist.Name}"
                    None
        | [] -> notifyError $"{nameof (OnRemoveSubTotal)}: no matches for {subTotalId}" None
        | _ ->
            notifyError $"{nameof (OnRemoveSubTotal)}: matches for {subTotalId} for multiple {nameof (Playlist)}s" None
    | OnPlayTrack trackId ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) -> state, Cmd.ofMsg (RequestTrack(trackData, playlist, true)), []
        | Error error -> notifyError $"{nameof (OnPlayTrack)}: {error}" None
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
                            do! Async.Sleep 250
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
            | _ -> noChange // TODO-NMB: No need to notify error for this?...notifyError $"{nameof (Seek)}: {nameof (PlayerState)} is not {nameof (Playing)}, {nameof (Paused)} or {nameof (Stopped)}" None
        | None -> noChange // TODO-NMB: No need to notify error for this?...notifyError $"{nameof (Seek)}: when {nameof (TrackState)} is {nameof (None)}" None
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
                | Error error -> notifyError $"{nameof (OnPrevious)}: {error}" None
            | None -> notifyError $"{nameof (OnPrevious)}: {nameof (TrackState)}.Previous is {nameof (None)}" None
        | None -> notifyError $"{nameof (OnPrevious)}: {nameof (TrackState)} is {nameof (None)}" None
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
                | Error error -> notifyError $"{nameof (OnNext)}: {error}" None
            | None -> notifyError $"{nameof (OnNext)}: {nameof (TrackState)}.Next is {nameof (None)}" None
        | None -> notifyError $"{nameof (OnNext)}: {nameof (TrackState)} is {nameof (None)}" None
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
            | AwaitingPlay _ -> notifyError $"{nameof (OnPlay)}: {nameof (PlayerState)} is {nameof (AwaitingPlay)}" None
            | Playing _ -> notifyError $"{nameof (OnPlay)}: {nameof (PlayerState)} already {nameof (Playing)}" None
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
                notifyError $"{nameof (OnPlay)}: {nameof (PlayerState)} is {nameof (PlaybackErrored)}" None
        | None -> notifyError $"{nameof (OnPlay)}: {nameof (TrackState)} is {nameof (None)}" None
    | OnPause ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing (position, _) ->
                player.Pause()

                { state with TrackState = Some { trackState with PlayerState = Paused position } },
                Cmd.none,
                [ NotifyTrackStateChanged ]
            | _ -> notifyError $"{nameof (OnPause)}: {nameof (PlayerState)} is not {nameof (Playing)}" None
        | None -> notifyError $"{nameof (OnPause)}: {nameof (TrackState)} is {nameof (None)}" None
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
                    $"{nameof (OnStop)}: {nameof (PlayerState)} is neither {nameof (Playing)} nor {nameof (Paused)}"
                    None
        | None -> notifyError $"{nameof (OnStop)}: {nameof (TrackState)} is {nameof (None)}" None
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
    // From App
    | NotifyNewPlaylistRequested ->
        let newPlaylist =
            { Id = PlaylistId.Create()
              Name = "new playlist"
              Items = [] }

        let newPlaylists = newPlaylist :: (state.Playlists |> List.rev) |> List.rev

        { state with
            Playlists = newPlaylists
            SelectedPlaylistId = Some newPlaylist.Id },
        Cmd.ofMsg (WritePlaylist newPlaylist.Id),
        [ NotifyNewPlaylistAdded newPlaylist ]
    | NotifyPlaylistOpened playlist ->
        let cmd =
            if state.Playlists.Length = 0 then
                Cmd.ofMsg (RequestDefaultTrack(Some playlist, false))
            else
                Cmd.none

        { state with
            Playlists = playlist :: (state.Playlists |> List.rev) |> List.rev
            SelectedPlaylistId = Some playlist.Id },
        cmd,
        [ NotifyPlaylistsChanged ]
    | NotifyFilesAdded files ->
        let tracks =
            files
            |> List.map FileInfo
            |> List.map (fun fi ->
                { Id = TrackId.Create()
                  Folder = fi.DirectoryName
                  Name = fi.Name
                  Duration = None })

        state, Cmd.ofMsg (AddToPlaylist tracks), []
    | NotifyFolderAdded folder ->
        let dottedFileExtensions = fileExtensions |> List.map (fun ext -> $".{ext}")

        let tracks =
            (DirectoryInfo folder).GetFiles()
            |> List.ofSeq
            |> List.filter (fun fi -> dottedFileExtensions |> List.contains fi.Extension)
            |> List.map (fun fi ->
                { Id = TrackId.Create()
                  Folder = fi.DirectoryName
                  Name = fi.Name
                  Duration = None })

        state, Cmd.ofMsg (AddToPlaylist tracks), []
    // From MediaPlayer subscriptions
    | NotifyErrored ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | AwaitingPlay _ ->
                { state with TrackState = Some { trackState with PlayerState = PlaybackErrored } },
                Cmd.none,
                [ NotifyTrackStateChanged ]
            | _ -> notifyError $"{nameof (NotifyErrored)}: {nameof (PlayerState)} is not {nameof (AwaitingPlay)}" None
        | None -> notifyError $"{nameof (NotifyErrored)}: {nameof (TrackState)} is {nameof (None)}" None
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
                match findTrack state.Playlists newTrackState.Track.Id with
                | Ok (playlist, _) ->
                    let mutable updated = 0

                    let newItems =
                        playlist.Items
                        |> List.map (fun item ->
                            if isTrackId newTrackState.Track.Id item then
                                updated <- updated + 1
                                Track newTrackState.Track
                            else
                                item)

                    if updated = 1 then
                        let newPlaylist = { playlist with Items = newItems }

                        match updatePlaylists state.Playlists newPlaylist with
                        | Ok newPlaylists ->
                            { state with
                                Playlists = newPlaylists
                                TrackState = Some newTrackState },
                            Cmd.ofMsg (WritePlaylist newPlaylist.Id),
                            externalMsgs
                        | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}" None
                    else if updated = 0 then
                        notifyError
                            $"{nameof (NotifyPlaying)}: no matches for {newTrackState.Track.Id} for {nameof (Playlist)} {playlist.Name}"
                            None
                    else
                        notifyError
                            $"{nameof (NotifyPlaying)}: multiple matches for {newTrackState.Track.Id} for {nameof (Playlist)} {playlist.Name}"
                            None
                | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}" None
            else
                { state with TrackState = Some newTrackState }, Cmd.none, externalMsgs
        | None -> notifyError $"{nameof (NotifyPlaying)}: {nameof (TrackState)} is {nameof (None)}" None
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
            | _ -> notifyError $"{nameof (NotifyPositionChanged)}: {nameof (PlayerState)} not {nameof (Playing)}" None
        | None -> notifyError $"{nameof (NotifyPositionChanged)}: {nameof (TrackState)} is {nameof (None)}" None
    | NotifyEnded ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _ ->
                let cmd =
                    match trackState.Next with
                    | Some _ -> Cmd.ofMsg OnNext
                    | _ -> Cmd.none

                { state with TrackState = Some { trackState with PlayerState = Ended } },
                cmd,
                [ NotifyTrackStateChanged ]
            | _ -> notifyError $"{nameof (NotifyEnded)}: {nameof (PlayerState)} not {nameof (Playing)}" None
        | None -> notifyError $"{nameof (NotifyEnded)}@ {nameof (TrackState)} is {nameof (None)}" None
