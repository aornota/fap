module Aornota.Fap.Player.Transition

open Aornota.Fap.Domain
open Aornota.Fap.Player.Model
open Aornota.Fap.Player.Utilities
open Aornota.Fap.Utilities
open Elmish
open LibVLCSharp.Shared
open System
open System.IO

type ExternalMsg =
    // For Playlists
    | RequestPrevious of TrackId * play: bool
    | RequestNext of TrackId * play: bool
    | NotifyPlaying of TrackId * duration: int64<millisecond>
    | NotifyPaused of TrackId
    | NotifyStopped of TrackId
    | NotifyEnded of TrackId
    | NotifyPlaybackErrored of TrackId
    // For App
    | NotifyTrack
    | NotifyNoTrack
    | NotifyMutedToggled
    | NotifyVolumeChanged
    | NotifyError of string

type Msg =
    // Internal
    | Seek of float32
    | DebounceSeekRequest of SeekRequestId * float32
    | Previous
    | Next
    | Play
    | Pause
    | Stop
    | ToggleMuted
    | Volume of int
    // From Playlists
    | NotifyTrackRequested of trackData: TrackData * hasPrevious: bool * hasNext: bool * play: bool
    // TODO-NMB...| NotifyTrackContextUpdated of trackId: TrackId * playlistName: string * hasPrevious: bool * hasNext: bool
    // From App
    | NotifyNoTrackRequested
    | NotifyPlaying
    | NotifyPaused
    | NotifyStopped
    | NotifyEnded
    | NotifyPositionChanged of float32
    | NotifyPlaybackErrored

[<Literal>]
let private DEBOUNCE_SEEK_REQUEST_DELAY = 250

let init muted volume =
    { Muted = muted
      Volume = volume
      TrackState = None
      SeekRequests = [] }

let transition msg (state: State) (player: MediaPlayer) =
    let playTrack track =
        use media = getMediaFromlocal (Path.Combine(track.Folder, track.Name))
        player.Play media |> ignore

    let isPlaying =
        function
        | Playing _ -> true
        | _ -> false

    let notifyError error =
        state, Cmd.none, [ NotifyError $"Player.transition -> {error}" ]

    let noChange = state, Cmd.none, []

    match msg with
    | Seek position ->
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
    | Previous ->
        match state.TrackState with
        | Some trackState when trackState.HasPrevious ->
            state, Cmd.none, [ RequestPrevious(trackState.Track.Id, isPlaying trackState.PlayerState) ]
        | Some _ -> notifyError $"{nameof (Previous)} when not trackState.HasPrevious"
        | None -> notifyError $"{nameof (Previous)} when trackState is {nameof (None)}"
    | Next ->
        match state.TrackState with
        | Some trackState when trackState.HasNext ->
            state, Cmd.none, [ RequestNext(trackState.Track.Id, isPlaying trackState.PlayerState) ]
        | Some _ -> notifyError $"{nameof (Next)} when not trackState.HasNext"
        | None -> notifyError $"{nameof (Next)} when trackState is {nameof (None)}"
    | Play ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | NoMedia
            | Ended ->
                playTrack trackState.Track

                { state with TrackState = Some { trackState with PlayerState = AwaitingPlay } }, Cmd.none, []
            | AwaitingPlay _ -> notifyError $"{nameof (Play)} when {nameof (PlayerState)} is {nameof (AwaitingPlay)}"
            | Playing _ -> notifyError $"{nameof (Play)} when {nameof (PlayerState)} already {nameof (Playing)}"
            | Paused position ->
                player.Pause()
                player.Position <- position

                { state with TrackState = Some { trackState with PlayerState = Playing(position, None) } }, Cmd.none, []
            | Stopped position ->
                player.Play() |> ignore
                player.Position <- position

                { state with TrackState = Some { trackState with PlayerState = Playing(position, None) } }, Cmd.none, []
            | PlaybackErrored ->
                notifyError $"{nameof (Play)} when {nameof (PlayerState)} is {nameof (PlaybackErrored)}"
        | None -> notifyError $"{nameof (Play)} when trackState is {nameof (None)}"
    | Pause ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing (position, _) ->
                player.Pause()
                { state with TrackState = Some { trackState with PlayerState = Paused position } }, Cmd.none, []
            | _ -> notifyError $"{nameof (Pause)} when {nameof (PlayerState)} is not {nameof (Playing)}"
        | None -> notifyError $"{nameof (Pause)} when trackState is {nameof (None)}"
    | Stop ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _
            | Paused _ ->
                player.Stop()

                { state with TrackState = Some { trackState with PlayerState = Stopped START_POSITION } }, Cmd.none, []
            | _ ->
                notifyError
                    $"{nameof (Stop)} when {nameof (PlayerState)} is not {nameof (Playing)} or {nameof (Paused)}"
        | None -> notifyError $"{nameof (Stop)} when trackState is {nameof (None)}"
    | ToggleMuted ->
        let newMuted = not state.Muted
        player.Mute <- newMuted
        { state with Muted = newMuted }, Cmd.none, [ NotifyMutedToggled ]
    | Volume volume ->
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
    | NotifyTrackRequested (trackData, hasPrevious, hasNext, play) ->
        let tryNewStateAndCmd =
            match state.TrackState with
            | Some trackState ->
                if trackState.Track.Id = trackData.Id then
                    if not play then
                        Error
                            $"{nameof (NotifyTrackRequested)} when trackState is already requested {nameof (TrackData)} and not play"
                    else
                        match trackState.PlayerState with
                        | Playing _ -> Ok(state, Cmd.ofMsg (Seek START_POSITION))
                        | _ -> Ok(state, Cmd.ofMsg Play)
                else
                    if play then playTrack trackData else player.Media <- null

                    Ok(
                        { state with
                            TrackState =
                                Some
                                    { Track = trackData
                                      PlayerState = (if play then AwaitingPlay else NoMedia)
                                      HasPrevious = hasPrevious
                                      HasNext = hasNext } },
                        Cmd.none
                    )
            | None ->
                if play then
                    playTrack trackData

                Ok(
                    { state with
                        TrackState =
                            Some
                                { Track = trackData
                                  PlayerState = (if play then AwaitingPlay else NoMedia)
                                  HasPrevious = hasPrevious
                                  HasNext = hasNext } },
                    Cmd.none
                )

        match tryNewStateAndCmd with
        | Ok (newState, cmd) -> newState, cmd, [ NotifyTrack ]
        | Error error -> notifyError error
    (* TODO-NMB...| NotifyTrackContextUpdated (trackId, playlistName, hasPrevious, hasNext) ->
        match state.TrackState with
        | Some trackState when trackState.Track.Id = trackId ->
            { state with
                TrackState =
                    Some
                        { trackState with
                            PlaylistName = playlistName
                            HasPrevious = hasPrevious
                            HasNext = hasNext } },
            Cmd.none,
            None
        | _ ->
            // TODO-NMB: Should this be an error?...
            noChange *)
    | NotifyNoTrackRequested ->
        player.Media <- null

        { state with
            TrackState = None
            SeekRequests = [] },
        Cmd.none,
        [ NotifyNoTrack ]
    | NotifyPlaying ->
        match state.TrackState with
        | Some trackState ->
            let duration = player.Length * 1L<millisecond>

            let newState =
                match trackState.PlayerState with
                | AwaitingPlay _ ->
                    { state with
                        TrackState =
                            Some
                                { trackState with
                                    Track = { trackState.Track with Duration = Some duration }
                                    PlayerState = Playing(START_POSITION, None) } }
                | _ -> state

            newState, Cmd.none, [ ExternalMsg.NotifyPlaying(trackState.Track.Id, duration) ]
        | None -> notifyError $"{nameof (NotifyPlaying)} when trackState is {nameof (None)}"
    | NotifyPaused ->
        match state.TrackState with
        | Some trackState -> state, Cmd.none, [ ExternalMsg.NotifyPaused trackState.Track.Id ]
        | None -> notifyError $"{nameof (NotifyPaused)} when trackState is {nameof (None)}"
    | NotifyStopped ->
        match state.TrackState with
        | Some trackState ->
            let externalMsg =
                if trackState.PlayerState <> PlaybackErrored then
                    [ ExternalMsg.NotifyStopped trackState.Track.Id ]
                else
                    []

            state, Cmd.none, externalMsg
        | None -> notifyError $"{nameof (NotifyStopped)} when trackState is {nameof (None)}"
    | NotifyEnded ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _ ->
                let externalMsg =
                    if trackState.HasNext then
                        RequestNext(trackState.Track.Id, true)
                    else
                        ExternalMsg.NotifyEnded trackState.Track.Id

                { state with TrackState = Some { trackState with PlayerState = Ended } }, Cmd.none, [ externalMsg ]
            | _ -> notifyError $"{nameof (NotifyEnded)} when {nameof (PlayerState)} not {nameof (Playing)}"
        | None -> notifyError $"{nameof (NotifyEnded)} when trackState is {nameof (None)}"
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
        | None -> notifyError $"{nameof (NotifyPositionChanged)} when trackState is {nameof (None)}"
    | NotifyPlaybackErrored ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | AwaitingPlay _ ->
                { state with TrackState = Some { trackState with PlayerState = PlaybackErrored } },
                Cmd.none,
                [ ExternalMsg.NotifyPlaybackErrored trackState.Track.Id ]
            | _ ->
                notifyError
                    $"{nameof (NotifyPlaybackErrored)} when {nameof (PlayerState)} is not {nameof (AwaitingPlay)}"
        | None -> notifyError $"{nameof (NotifyPlaybackErrored)} when trackState is {nameof (None)}"
