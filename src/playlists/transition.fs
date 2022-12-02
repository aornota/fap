module Aornota.Fap.Playlists.Transition

open Aornota.Fap
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
    // From Simulation
    | SimulationMsg of Simulation.Transition.Msg
    // From App
    | NotifyNewPlaylistRequested
    | NotifyPlaylistOpened of Playlist
    | NotifyFilesAdded of string list
    | NotifyFolderAdded of string
    | NotifyToggleShowSimulation
    // From MediaPlayer subscriptions
    | NotifyErrored
    | NotifyPlaying
    | NotifyPositionChanged of float32
    | NotifyEnded

[<Literal>]
let private ERROR_PREFIX = "Playlists -> "

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
    | None -> Error $"no matches for {trackId} for {playlist.Name}"

let private updatePlaylists (playlists: Playlist list) (playlist: Playlist) =
    let mutable updated = 0

    let updatedPlaylists =
        playlists
        |> List.map (fun existingPlaylist ->
            if existingPlaylist.Id = playlist.Id then
                updated <- updated + 1
                playlist
            else
                existingPlaylist)

    if updated = 1 then
        Ok updatedPlaylists
    else if updated = 0 then
        Error $"no matches for {playlist.Id}"
    else
        Error $"multiple matches for {playlist.Id}"

let private playTrack (track: TrackData) (player: MediaPlayer) =
    let path = Path.Combine(track.Folder, track.Name)
    use libvlc = new LibVLC()
    use media = new Media(libvlc, path, FromType.FromPath)
    player.Play media |> ignore

let private canEvolveSimulation =
    function
    | Simulation.Model.EvolutionState.Paused
    | Simulation.Model.EvolutionState.Reset
    | Simulation.Model.EvolutionState.InvalidConfiguration -> true
    | _ -> false

let private canPauseSimulation =
    function
    | Simulation.Model.EvolutionState.Evolving -> true
    | _ -> false

let private canResetSimulation =
    function
    | Simulation.Model.EvolutionState.Evolving
    | Simulation.Model.EvolutionState.Paused -> true
    | _ -> false

let init playlistIds lastTrackId muted volume autoPlay showSimulation (player: MediaPlayer) =
    player.Media <- null

    let simulationState =
        if showSimulation then
            Some(Simulation.Transition.init ())
        else
            None

    { Playlists = []
      SelectedPlaylistId = None
      Muted = muted
      Volume = volume
      TrackState = None
      WritePlaylistRequests = []
      SeekRequests = []
      SimulationState = simulationState },
    [ RequestNoTrack
      ReadPlaylists(playlistIds |> List.rev, lastTrackId, autoPlay, []) ]

let update msg state (player: MediaPlayer) =
    let notifyError error nonDebugMessage =
        state, Cmd.none, [ NotifyError($"{ERROR_PREFIX}{error}", nonDebugMessage) ]

    let noChange = state, Cmd.none, []

    match msg with
    // Internal
    | NoOp -> noChange
    | ReadPlaylists (playlistIdsToRead, lastTrackId, autoPlay, playlists) ->
        match playlistIdsToRead with
        | playlistId :: playlistIdsToRead ->
            let read () =
                async { return! readPlaylist playlistId }

            let handleResult =
                function
                | Ok playlist -> ReadPlaylists(playlistIdsToRead, lastTrackId, autoPlay, playlist :: playlists)
                | Error error ->
                    ReadPlaylistError(error, playlistId, playlistIdsToRead, lastTrackId, autoPlay, playlists)

            state, Cmd.OfAsync.perform read () handleResult, []
        | [] ->
            let selectedPlaylistIdAndCmdAndExternalMsgs =
                match lastTrackId with
                | Some lastTrackId ->
                    match findTrack playlists lastTrackId with
                    | Ok (playlist, trackData) ->
                        Some(playlist.Id, Cmd.ofMsg (RequestTrack(trackData, playlist, autoPlay))), []
                    | Error error -> None, [ NotifyError($"{ERROR_PREFIX}{nameof (ReadPlaylists)}: {error}", None) ]
                | None -> None, []

            let selectedPlaylistId, cmd, externalMsgs =
                match selectedPlaylistIdAndCmdAndExternalMsgs with
                | (Some selectedPlaylistIdAndMsg, externalMsgs) ->
                    Some(fst selectedPlaylistIdAndMsg), snd selectedPlaylistIdAndMsg, externalMsgs
                | (None, externalMsgs) -> None, Cmd.ofMsg (RequestDefaultTrack(None, autoPlay)), externalMsgs

            { state with
                Playlists = playlists
                SelectedPlaylistId = selectedPlaylistId },
            cmd,
            NotifyPlaylistsChanged :: externalMsgs
    | ReadPlaylistError (readError, playlistId, playlistIdsToRead, lastTrackId, autoPlay, playlists) ->
        let (PlaylistId guid) = playlistId

        state,
        Cmd.ofMsg (ReadPlaylists(playlistIdsToRead, lastTrackId, autoPlay, playlists)),
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
        let updatedWritePlaylistPlayerRequests =
            state.WritePlaylistRequests
            |> List.filter (fun (existingWritePlaylistRequestId, _) ->
                existingWritePlaylistRequestId <> writePlaylistRequestId)

        match
            updatedWritePlaylistPlayerRequests
            |> List.filter (fun (_, existingPlaylistId) -> existingPlaylistId = playlistId)
        with
        | _ :: _ -> { state with WritePlaylistRequests = updatedWritePlaylistPlayerRequests }, Cmd.none, []
        | [] ->
            let writePlaylist playlistId =
                async {
                    match
                        state.Playlists
                        |> List.tryFind (fun existingPlaylist -> existingPlaylist.Id = playlistId)
                    with
                    | Some playlist -> return! writePlaylist playlist
                    | None -> return Error "Playlist not found"
                }

            let handleResult =
                function
                | Ok _ -> NoOp
                | Error error -> WritePlaylistError(error, playlistId)

            { state with WritePlaylistRequests = updatedWritePlaylistPlayerRequests },
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
                let updatedSeekRequests =
                    state.SeekRequests
                    |> List.filter (fun existingSeekRequestId -> existingSeekRequestId <> seekRequestId)

                match updatedSeekRequests with
                | _ :: _ -> { state with SeekRequests = updatedSeekRequests }, Cmd.none, []
                | [] ->
                    player.Position <- position

                    { state with
                        TrackState = Some { trackState with PlayerState = Playing(position, Some position) }
                        SeekRequests = updatedSeekRequests },
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
                    |> List.filter (fun otherPlaylist -> tracks otherPlaylist |> List.length > 0)
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
            let updatedStateAndCmdAndExternalMsgs =
                let randomConfigurationCmd =
                    Cmd.map SimulationMsg (Cmd.ofMsg Simulation.Transition.Msg.OnRandomConfiguration)

                let simulationsCmds =
                    match state.SimulationState with
                    | Some simulationState when canResetSimulation simulationState.EvolutionState ->
                        Cmd.batch
                            [ Cmd.map SimulationMsg (Cmd.ofMsg Simulation.Transition.Msg.OnReset)
                              randomConfigurationCmd ]
                    | Some _ -> randomConfigurationCmd
                    | None -> Cmd.none

                match state.TrackState with
                | Some trackState ->
                    if trackState.Track.Id = trackData.Id then
                        if not play then
                            Error
                                $"{nameof (RequestTrack)}: {nameof (TrackState)} is already requested {nameof (TrackData)} and not play"
                        else
                            match trackState.PlayerState with
                            | Playing _ ->
                                Ok(
                                    { state with SeekRequests = [] },
                                    Cmd.batch [ Cmd.ofMsg (OnSeek START_POSITION); simulationsCmds ],
                                    []
                                )
                            | _ -> Ok(state, Cmd.batch [ Cmd.ofMsg OnPlay; simulationsCmds ], [])
                    else
                        if play then
                            playTrack trackData player
                        else
                            player.Media <- null

                        let updatedTrackState =
                            { Track = trackData
                              PlayerState = (if play then AwaitingPlay else NoMedia)
                              Previous = previous
                              Next = next }

                        Ok(
                            { state with
                                TrackState = Some updatedTrackState
                                SeekRequests = [] },
                            simulationsCmds,
                            [ NotifyTrackStateChanged ]
                        )
                | None ->
                    if play then
                        playTrack trackData player

                    let updatedTrackState =
                        { Track = trackData
                          PlayerState = (if play then AwaitingPlay else NoMedia)
                          Previous = previous
                          Next = next }

                    Ok(
                        { state with
                            TrackState = Some updatedTrackState
                            SeekRequests = [] },
                        simulationsCmds,
                        [ NotifyTrackStateChanged ]
                    )

            match updatedStateAndCmdAndExternalMsgs with
            | Ok (newState, cmd, externalMsgs) -> newState, cmd, externalMsgs
            | Error error -> notifyError error None
        | Error error -> notifyError $"{nameof (RequestTrack)}: {error}" None
    | RequestNoTrack ->
        let simulationCmd =
            match state.SimulationState with
            | Some simulationState when canResetSimulation simulationState.EvolutionState ->
                Cmd.map SimulationMsg (Cmd.ofMsg Simulation.Transition.Msg.OnReset)
            | _ -> Cmd.none

        player.Media <- null

        { state with
            TrackState = None
            SeekRequests = [] },
        simulationCmd,
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

                    let updatedPlaylist = { playlist with Items = (playlist.Items @ items) |> sanitize }

                    let writePlaylistCmd = Cmd.ofMsg (WritePlaylist playlist.Id)

                    match updatePlaylists state.Playlists updatedPlaylist with
                    | Ok updatedPlaylists ->
                        let cmd =
                            match requestTrack with
                            | Some trackData ->
                                Cmd.batch
                                    [ Cmd.ofMsg (RequestTrack(trackData, updatedPlaylist, false))
                                      writePlaylistCmd ]
                            | None -> Cmd.batch [ Cmd.ofMsg (UpdatePreviousAndNext updatedPlaylist); writePlaylistCmd ]

                        { state with Playlists = updatedPlaylists }, cmd, [ NotifyTracksAddedOrRemoved updatedPlaylist ]
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
                let (_, updatedPlaylists, _) =
                    let index =
                        match horizontalDirection with
                        | Left -> index - 1
                        | Right -> index

                    state.Playlists
                    |> List.fold
                        (fun (i, updatedPlaylists, pending) playlist ->
                            let updatedPlaylists, pending =
                                match pending with
                                | Some pending -> pending :: playlist :: updatedPlaylists, None
                                | None when i = index -> updatedPlaylists, Some playlist
                                | None -> playlist :: updatedPlaylists, None

                            i + 1, updatedPlaylists, pending)
                        (0, [], None)

                { state with Playlists = updatedPlaylists |> List.rev }, Cmd.none, [ NotifyPlaylistsChanged ]
        | None -> notifyError $"{nameof (OnMovePlaylist)} {horizontalDirection} ({playlistId}): not found" None
    | OnRemovePlaylist playlistId ->
        let playlist, nextOrPrevious =
            state.Playlists
            |> List.rev
            |> List.fold
                (fun (playlist, nextOrPrevious) existingPlaylist ->
                    if existingPlaylist.Id = playlistId then
                        Some existingPlaylist, nextOrPrevious
                    else
                        match playlist, nextOrPrevious with
                        | None, _ -> None, Some existingPlaylist
                        | Some _, None -> playlist, Some existingPlaylist
                        | Some _, Some _ -> playlist, nextOrPrevious)
                (None, None)

        match playlist with
        | Some playlist ->
            let updatedPlaylists =
                state.Playlists
                |> List.filter (fun existingPlaylist -> existingPlaylist.Id <> playlist.Id)

            let hasCurrentTrack, isPlaying =
                match state.TrackState with
                | Some trackState ->
                    if
                        tracks playlist
                        |> List.exists (fun trackData -> trackData.Id = trackState.Track.Id)
                    then
                        match trackState.PlayerState with
                        | Playing _ -> true, true
                        | _ -> true, false
                    else
                        false, false
                | None -> false, false

            let updatedSelectedPlaylistId =
                nextOrPrevious |> Option.map (fun playlist -> playlist.Id)

            let cmd =
                match hasCurrentTrack, nextOrPrevious with
                | true, Some nextOrPrevious ->
                    Cmd.batch
                        [ Cmd.ofMsg RequestNoTrack
                          Cmd.ofMsg (RequestDefaultTrack(Some nextOrPrevious, isPlaying)) ]
                | true, None -> Cmd.ofMsg RequestNoTrack
                | _ -> Cmd.none

            { state with
                Playlists = updatedPlaylists
                SelectedPlaylistId = updatedSelectedPlaylistId },
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
                    | Track existingTrackData when existingTrackData.Id = trackData.Id -> true
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

                    let (_, updatedItems, _) =
                        playlist.Items
                        |> List.fold
                            (fun (i, updatedItems, pending) item ->
                                let updatedItems, pending =
                                    match pending with
                                    | Some pending -> pending :: item :: updatedItems, None
                                    | None when i = index -> updatedItems, Some item
                                    | None -> item :: updatedItems, None

                                i + 1, updatedItems, pending)
                            (0, [], None)

                    let updatedPlaylist = { playlist with Items = updatedItems |> List.rev |> sanitize }

                    let trackStateForPlaylist =
                        match state.TrackState with
                        | Some trackState ->
                            if
                                tracks updatedPlaylist
                                |> List.exists (fun existingTrackData -> existingTrackData.Id = trackState.Track.Id)
                            then
                                Some trackState
                            else
                                None
                        | None -> None

                    let writePlaylistCmd = Cmd.ofMsg (WritePlaylist playlist.Id)

                    match updatePlaylists state.Playlists updatedPlaylist with
                    | Ok updatedPlaylists ->
                        match trackStateForPlaylist with
                        | Some trackStateForPlaylist ->
                            match previousAndNext updatedPlaylist trackStateForPlaylist.Track.Id with
                            | Ok (previous, next) ->
                                let updatedTrackState =
                                    { trackStateForPlaylist with
                                        Previous = previous
                                        Next = next }

                                { state with
                                    Playlists = updatedPlaylists
                                    TrackState = Some updatedTrackState },
                                writePlaylistCmd,
                                []
                            | Error error ->
                                notifyError $"{nameof (OnMoveTrack)} {verticalDirection} ({trackData.Id}): {error}" None
                        | None -> { state with Playlists = updatedPlaylists }, writePlaylistCmd, []
                    | Error error ->
                        notifyError $"{nameof (OnMoveTrack)} {verticalDirection} ({trackData.Id}): {error}" None
            | _ -> notifyError $"{nameof (OnMoveTrack)} {verticalDirection} ({trackData.Id}): not found" None
        | Error error -> notifyError $"{nameof (OnMoveTrack)} {verticalDirection} ({trackId}): {error}" None
    | OnMoveTrack (trackId, Horizontal (horizontalDirection: HorizontalDirection)) ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) ->
            match
                state.Playlists
                |> List.tryFindIndex (fun existingPlaylist -> existingPlaylist.Id = playlist.Id)
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
                    let destinationPlaylist =
                        match horizontalDirection with
                        | Left -> state.Playlists[index - 1]
                        | Right -> state.Playlists[index + 1]

                    let updatedItems =
                        playlist.Items |> List.filter (fun item -> not (isTrackId trackData.Id item))

                    let updatedDestinationItems =
                        Track trackData
                        :: SubTotal(SubTotalId.Create()) :: (destinationPlaylist.Items |> List.rev)
                        |> List.rev

                    let updatedPlaylist, updatedDestinationPlaylist =
                        { playlist with Items = updatedItems |> sanitize },
                        { destinationPlaylist with Items = updatedDestinationItems |> sanitize }

                    let trackStateForPlaylist =
                        match state.TrackState with
                        | Some trackState ->
                            if
                                tracks updatedPlaylist
                                |> List.exists (fun existingTrackData -> existingTrackData.Id = trackState.Track.Id)
                            then
                                Some(trackState, updatedPlaylist)
                            else if
                                tracks updatedDestinationPlaylist
                                |> List.exists (fun existingTrackData -> existingTrackData.Id = trackState.Track.Id)
                            then
                                Some(trackState, updatedDestinationPlaylist)
                            else
                                None
                        | None -> None

                    let writePlaylistCmd = Cmd.ofMsg (WritePlaylist playlist.Id)

                    match updatePlaylists state.Playlists updatedPlaylist with
                    | Ok updatedPlaylists ->
                        match updatePlaylists updatedPlaylists updatedDestinationPlaylist with
                        | Ok updatedPlaylists ->
                            match trackStateForPlaylist with
                            | Some (trackStateForPlaylist, playlist) ->
                                match previousAndNext playlist trackStateForPlaylist.Track.Id with
                                | Ok (previous, next) ->
                                    let newTrackState =
                                        { trackStateForPlaylist with
                                            Previous = previous
                                            Next = next }

                                    { state with
                                        Playlists = updatedPlaylists
                                        TrackState = Some newTrackState },
                                    writePlaylistCmd,
                                    []
                                | Error error ->
                                    notifyError $"{nameof (OnMoveTrack)} {horizontalDirection}: {error}" None
                            | None -> { state with Playlists = updatedPlaylists }, writePlaylistCmd, []
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
                    let updatedItems =
                        Track trackData :: SubTotal(SubTotalId.Create()) :: (items |> List.tail)

                    let updatedPlaylist = { playlist with Items = updatedItems |> List.rev |> sanitize }

                    match updatePlaylists state.Playlists updatedPlaylist with
                    | Ok updatedPlaylists ->
                        { state with Playlists = updatedPlaylists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
                    | Error error -> notifyError $"{nameof (OnAddSubTotal)} {nameof (Below)} ({trackId}): {error}" None
                | _ ->
                    notifyError
                        $"{nameof (OnAddSubTotal)} {nameof (Above)} ({trackId}): not the last item in the {nameof (Playlist)}"
                        None
            | Below ->
                match playlist.Items |> List.head with
                | Track trackData when trackData.Id = trackId ->
                    let updatedItems =
                        Track trackData
                        :: SubTotal(SubTotalId.Create()) :: (playlist.Items |> List.tail)

                    let updatedPlaylist = { playlist with Items = updatedItems |> sanitize }

                    match updatePlaylists state.Playlists updatedPlaylist with
                    | Ok updatedPlaylists ->
                        { state with Playlists = updatedPlaylists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
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

                let updatedItems =
                    playlist.Items |> List.filter (fun item -> isTrackId trackData.Id item |> not)

                let updatedPlaylist = { playlist with Items = updatedItems |> sanitize }

                let writePlaylistCmd = Cmd.ofMsg (WritePlaylist playlist.Id)

                match updatePlaylists state.Playlists updatedPlaylist with
                | Ok updatedPlaylists ->
                    let cmd =
                        match isCurrentTrack, previous, next with
                        | true, _, Some next ->
                            Cmd.batch
                                [ Cmd.ofMsg RequestNoTrack
                                  writePlaylistCmd
                                  Cmd.ofMsg (RequestTrack(next, updatedPlaylist, isPlaying)) ]
                        | true, Some previous, _ ->
                            Cmd.batch
                                [ Cmd.ofMsg RequestNoTrack
                                  writePlaylistCmd
                                  Cmd.ofMsg (RequestTrack(previous, updatedPlaylist, isPlaying)) ]
                        | true, None, None -> Cmd.batch [ Cmd.ofMsg RequestNoTrack; writePlaylistCmd ]
                        | _ -> Cmd.batch [ Cmd.ofMsg (UpdatePreviousAndNext updatedPlaylist); writePlaylistCmd ]

                    { state with Playlists = updatedPlaylists }, cmd, [ NotifyTracksAddedOrRemoved updatedPlaylist ]
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
                        | SubTotal existingSubTotalId when existingSubTotalId = subTotalId -> Some item
                        | _ -> None)

                match subTotalMatches with
                | _ :: _ -> Some(playlist, subTotalMatches)
                | [] -> None)

        match matches with
        | [ playlist, subTotalMatches ] ->
            match subTotalMatches with
            | [ _ ] ->
                let updatedItems =
                    playlist.Items
                    |> List.filter (fun item ->
                        match item with
                        | SubTotal existingSubTotalId when existingSubTotalId = subTotalId -> false
                        | _ -> true)

                let updatedPlaylist = { playlist with Items = updatedItems |> sanitize }

                match updatePlaylists state.Playlists updatedPlaylist with
                | Ok updatedPlaylists ->
                    { state with Playlists = updatedPlaylists }, Cmd.ofMsg (WritePlaylist playlist.Id), []
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
                        // Note: Avoid glitches due to rounding errors (e.g. NotifyPositionChanged cam cause Slider to call RequestSeek).
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
            | _ -> noChange // note: no need to treat as an error
        | None -> noChange // note: no need to treat as an error
    | OnPrevious ->
        match state.TrackState with
        | Some trackState ->
            match trackState.Previous with
            | Some previous ->
                match findTrack state.Playlists previous.Id with
                | Ok (playlist, _) ->
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
                | Ok (playlist, _) ->
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

                { state with
                    TrackState = Some { trackState with PlayerState = AwaitingPlay }
                    SeekRequests = [] },
                Cmd.none,
                [ NotifyTrackStateChanged ]
            | AwaitingPlay -> notifyError $"{nameof (OnPlay)}: {nameof (PlayerState)} is {nameof (AwaitingPlay)}" None
            | Playing _ -> notifyError $"{nameof (OnPlay)}: {nameof (PlayerState)} already {nameof (Playing)}" None
            | Paused position ->
                player.Pause()
                player.Position <- position

                { state with
                    TrackState = Some { trackState with PlayerState = Playing(position, None) }
                    SeekRequests = [] },
                Cmd.none,
                [ NotifyTrackStateChanged ]
            | Stopped position ->
                player.Play() |> ignore
                player.Position <- position

                { state with
                    TrackState = Some { trackState with PlayerState = Playing(position, None) }
                    SeekRequests = [] },
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
                let simulationCmd =
                    match state.SimulationState with
                    | Some simulationState when canPauseSimulation simulationState.EvolutionState ->
                        Cmd.map SimulationMsg (Cmd.ofMsg Simulation.Transition.Msg.OnPause)
                    | _ -> Cmd.none

                player.Pause()

                { state with TrackState = Some { trackState with PlayerState = Paused position } },
                simulationCmd,
                [ NotifyTrackStateChanged ]
            | _ -> notifyError $"{nameof (OnPause)}: {nameof (PlayerState)} is not {nameof (Playing)}" None
        | None -> notifyError $"{nameof (OnPause)}: {nameof (TrackState)} is {nameof (None)}" None
    | OnStop ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _
            | Paused _ ->
                let simulationCmd =
                    match state.SimulationState with
                    | Some simulationState when canResetSimulation simulationState.EvolutionState ->
                        Cmd.map SimulationMsg (Cmd.ofMsg Simulation.Transition.Msg.OnReset)
                    | _ -> Cmd.none

                player.Stop()

                { state with TrackState = Some { trackState with PlayerState = Stopped START_POSITION } },
                simulationCmd,
                [ NotifyTrackStateChanged ]
            | _ ->
                notifyError
                    $"{nameof (OnStop)}: {nameof (PlayerState)} is neither {nameof (Playing)} nor {nameof (Paused)}"
                    None
        | None -> notifyError $"{nameof (OnStop)}: {nameof (TrackState)} is {nameof (None)}" None
    | OnToggleMuted ->
        let updatedMuted = not state.Muted
        player.Mute <- updatedMuted
        { state with Muted = updatedMuted }, Cmd.none, [ NotifyMutedToggled ]
    | OnVolume volume ->
        if volume <> state.Volume then
            let updatedMuted = volume = 0
            player.Mute <- updatedMuted
            player.Volume <- playerVolume volume

            let externalMsgs =
                [ NotifyVolumeChanged
                  if updatedMuted <> state.Muted then
                      NotifyMutedToggled ]

            { state with
                Muted = updatedMuted
                Volume = volume },
            Cmd.none,
            externalMsgs
        else
            noChange
    // From Simulation
    | SimulationMsg simulationMsg ->
        let handleExternal externalMsg =
            match externalMsg with
            | Simulation.Transition.ExternalMsg.NotifyError (error, nonDebugMessage) ->
                NotifyError(error, nonDebugMessage)

        match state.SimulationState with
        | Some simulationState ->
            let updatedSimulationState, cmd, externalMsgs =
                Simulation.Transition.update simulationMsg simulationState

            { state with SimulationState = Some updatedSimulationState },
            Cmd.map SimulationMsg cmd,
            externalMsgs |> List.map handleExternal
        | None ->
            match simulationMsg with
            | Simulation.Transition.Msg.Evolved _ -> noChange
            | _ -> notifyError $"{nameof (SimulationMsg)} ({simulationMsg}) whwn not showing {nameof (Simulation)}" None
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
    | NotifyToggleShowSimulation ->
        match state.SimulationState with
        | Some _ -> { state with SimulationState = None }, Cmd.none, []
        | None ->
            let simulationCmd =
                match state.TrackState with
                | Some trackState ->
                    match trackState.PlayerState with
                    | Playing _ -> Cmd.map SimulationMsg (Cmd.ofMsg Simulation.Transition.Msg.OnEvolve)
                    | _ -> Cmd.none
                | None -> Cmd.none

            { state with SimulationState = Some(Simulation.Transition.init ()) }, simulationCmd, []
    // From MediaPlayer subscriptions
    | NotifyErrored ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | AwaitingPlay ->
                let simulationMsg =
                    match state.SimulationState with
                    | Some _ -> Cmd.map SimulationMsg (Cmd.ofMsg Simulation.Transition.Msg.OnReset)
                    | None -> Cmd.none

                { state with TrackState = Some { trackState with PlayerState = PlaybackErrored } },
                simulationMsg,
                [ NotifyTrackStateChanged ]
            | _ -> notifyError $"{nameof (NotifyErrored)}: {nameof (PlayerState)} is not {nameof (AwaitingPlay)}" None
        | None -> notifyError $"{nameof (NotifyErrored)}: {nameof (TrackState)} is {nameof (None)}" None
    | NotifyPlaying ->
        match state.TrackState with
        | Some trackState ->
            let duration = player.Length * 1L<millisecond>

            let updatedTrackState, simulationCmd, externalMsgs =
                match trackState.PlayerState with
                | AwaitingPlay ->
                    let simulationMsg =
                        match state.SimulationState with
                        | Some simulationState when canEvolveSimulation simulationState.EvolutionState ->
                            Cmd.map SimulationMsg (Cmd.ofMsg Simulation.Transition.Msg.OnEvolve)
                        | _ -> Cmd.none

                    { trackState with
                        Track = { trackState.Track with Duration = Some duration }
                        PlayerState = Playing(START_POSITION, None) },
                    simulationMsg,
                    [ NotifyTrackStateChanged ]
                | _ -> trackState, Cmd.none, []

            if trackState.Track.Duration <> Some duration then
                match findTrack state.Playlists updatedTrackState.Track.Id with
                | Ok (playlist, _) ->
                    let mutable updated = 0

                    let updatedItems =
                        playlist.Items
                        |> List.map (fun item ->
                            if isTrackId updatedTrackState.Track.Id item then
                                updated <- updated + 1
                                Track updatedTrackState.Track
                            else
                                item)

                    if updated = 1 then
                        let updatedPlaylist = { playlist with Items = updatedItems |> sanitize }

                        match updatePlaylists state.Playlists updatedPlaylist with
                        | Ok updatedPlaylists ->
                            { state with
                                Playlists = updatedPlaylists
                                TrackState = Some updatedTrackState },
                            Cmd.batch [ simulationCmd; Cmd.ofMsg (WritePlaylist updatedPlaylist.Id) ],
                            externalMsgs
                        | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}" None
                    else if updated = 0 then
                        notifyError
                            $"{nameof (NotifyPlaying)}: no matches for {updatedTrackState.Track.Id} for {nameof (Playlist)} {playlist.Name}"
                            None
                    else
                        notifyError
                            $"{nameof (NotifyPlaying)}: multiple matches for {updatedTrackState.Track.Id} for {nameof (Playlist)} {playlist.Name}"
                            None
                | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}" None
            else
                { state with TrackState = Some updatedTrackState }, simulationCmd, externalMsgs
        | None -> notifyError $"{nameof (NotifyPlaying)}: {nameof (TrackState)} is {nameof (None)}" None
    | NotifyPositionChanged position ->
        match state.TrackState with
        | Some trackState ->
            let simulationCmd =
                match state.SimulationState with
                | Some simulationState when canEvolveSimulation simulationState.EvolutionState ->
                    Cmd.map SimulationMsg (Cmd.ofMsg Simulation.Transition.Msg.OnEvolve)
                | _ -> Cmd.none

            match trackState.PlayerState with
            | AwaitingPlay ->
                { state with TrackState = Some { trackState with PlayerState = Playing(position, Some position) } },
                simulationCmd,
                []
            | Playing (currentPosition, _) ->
                let updatedPosition =
                    match state.SeekRequests with
                    | _ :: _ -> currentPosition
                    | [] -> position

                { state with TrackState = Some { trackState with PlayerState = Playing(updatedPosition, Some position) } },
                simulationCmd,
                []
            | _ ->
                notifyError
                    $"{nameof (NotifyPositionChanged)}: {nameof (PlayerState)} ({trackState.PlayerState}) is not {nameof (Playing)}"
                    None
        | None -> notifyError $"{nameof (NotifyPositionChanged)}: {nameof (TrackState)} is {nameof (None)}" None
    | NotifyEnded ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _ ->
                let cmd =
                    match trackState.Next with
                    | Some _ -> Cmd.ofMsg OnNext
                    | _ ->
                        match state.SimulationState with
                        | Some simulationState when canResetSimulation simulationState.EvolutionState ->
                            Cmd.map SimulationMsg (Cmd.ofMsg Simulation.Transition.Msg.OnReset)
                        | _ -> Cmd.none

                { state with TrackState = Some { trackState with PlayerState = Ended } },
                cmd,
                [ NotifyTrackStateChanged ]
            | _ -> notifyError $"{nameof (NotifyEnded)}: {nameof (PlayerState)} not {nameof (Playing)}" None
        | None -> notifyError $"{nameof (NotifyEnded)}@ {nameof (TrackState)} is {nameof (None)}" None
