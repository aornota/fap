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
    | RequestPrevious of trackId: TrackId * play: bool
    | RequestNext of trackId: TrackId * play: bool
    | NotifyPlaying of trackId: TrackId * duration: int64<millisecond>
    | NotifyPaused of trackId: TrackId
    | NotifyStopped of trackId: TrackId
    | NotifyEnded of trackId: TrackId
    | NotifyPlaybackErrored of trackId: TrackId
    | NotifyError of string
// TODO-NMB: Save preferences (muted / volume / &c.)?...

type Msg =
    | TrackSelected of track: TrackData * playlistName: string * hasPrevious: bool * hasNext: bool
    | PlayRequested of track: TrackData * playlistName: string * hasPrevious: bool * hasNext: bool
    | TrackContextUpdated of trackId: TrackId * playlistName: string * hasPrevious: bool * hasNext: bool
    | Seek of float32
    | DebounceSeekRequest of SeekRequestId * float32
    | Previous
    | Next
    | Play
    | Pause
    | Stop
    | ToggleMuted
    | NotifyPlaying
    | NotifyPaused
    | NotifyStopped
    | NotifyEnded
    | NotifyPositionChanged of float32
    | NotifyPlaybackErrored

[<Literal>]
let private DEBOUNCE_SEEK_REQUEST_DELAY = 250

let init muted =
    { Muted = muted
      TrackState = None
      SeekRequests = [] }

let transition msg (state: State) (player: MediaPlayer) =
    let play track =
        use media = getMediaFromlocal (Path.Combine(track.Folder, track.Name))
        player.Play media |> ignore

    let isPlaying =
        function
        | Playing _ -> true
        | _ -> false

    let notifyError message =
        state, Cmd.none, Some(NotifyError $"Player.transition -> {message}")

    let noChange = state, Cmd.none, None

    match msg with
    | TrackSelected (track, playlistName, hasPrevious, hasNext) ->
        let newState =
            match state.TrackState with
            | Some trackState ->
                match trackState.PlayerState, trackState.Track.Id = track.Id with
                | NoMedia, false ->
                    { state with
                        TrackState =
                            Some
                                { trackState with
                                    Track = track
                                    PlaylistName = playlistName
                                    HasPrevious = hasPrevious
                                    HasNext = hasNext } }
                | _ -> state
            | None ->
                { state with
                    TrackState =
                        Some
                            { Track = track
                              PlaylistName = playlistName
                              PlayerState = NoMedia
                              HasPrevious = hasPrevious
                              HasNext = hasNext } }

        newState, Cmd.none, None
    | PlayRequested (track, playlistName, hasPrevious, hasNext) ->
        play track

        { state with
            TrackState =
                Some
                    { Track = track
                      PlaylistName = playlistName
                      PlayerState = AwaitingPlay
                      HasPrevious = hasPrevious
                      HasNext = hasNext } },
        Cmd.none,
        None
    | TrackContextUpdated (trackId, playlistName, hasPrevious, hasNext) ->
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
            noChange
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
                    None
                else
                    noChange
            | Paused _ ->
                { state with TrackState = Some { trackState with PlayerState = Paused position } }, Cmd.none, None
            | Stopped _ ->
                { state with TrackState = Some { trackState with PlayerState = Stopped position } }, Cmd.none, None
            | _ -> noChange // No need to notify error for this...notifyError "RequestSeek when trackState.PlayerState not Playing"
        | None -> noChange // No need to notify error for this...notifyError "RequestSeek when trackState is None"
    | DebounceSeekRequest (seekRequestId, position) ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _ ->
                let newSeekRequests =
                    state.SeekRequests
                    |> List.filter (fun otherSeekRequestId -> otherSeekRequestId <> seekRequestId)

                match newSeekRequests with
                | _ :: _ -> { state with SeekRequests = newSeekRequests }, Cmd.none, None
                | [] ->
                    player.Position <- position

                    { state with
                        TrackState = Some { trackState with PlayerState = Playing(position, Some position) }
                        SeekRequests = newSeekRequests },
                    Cmd.none,
                    None
            | _ -> notifyError "DebounceSeekRequest when trackState.PlayerState not Playing"
        | None -> notifyError "DebounceSeekRequest when trackState is None"
    | Previous ->
        match state.TrackState with
        | Some trackState when trackState.HasPrevious ->
            state, Cmd.none, Some(ExternalMsg.RequestPrevious(trackState.Track.Id, isPlaying trackState.PlayerState))
        | Some _ -> notifyError "RequestPrevious when not trackState.HasPrevious"
        | None -> notifyError "RequestPrevious when trackState is None"
    | Next ->
        match state.TrackState with
        | Some trackState when trackState.HasNext ->
            state, Cmd.none, Some(ExternalMsg.RequestNext(trackState.Track.Id, isPlaying trackState.PlayerState))
        | Some _ -> notifyError "RequestNext when not trackState.HasNext"
        | None -> notifyError "RequestNext when trackState is None"
    | Play ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | NoMedia
            | Ended ->
                play trackState.Track

                { state with TrackState = Some { trackState with PlayerState = AwaitingPlay } }, Cmd.none, None
            | AwaitingPlay _ -> notifyError "RequestPlay when trackState.PlayerState is AwaitingPlay"
            | Playing _ -> notifyError "RequestPlay when trackState.PlayerState already Playing"
            | Paused position ->
                player.Pause()
                player.Position <- position

                { state with TrackState = Some { trackState with PlayerState = Playing(position, None) } },
                Cmd.none,
                None
            | Stopped position ->
                player.Play() |> ignore
                player.Position <- position

                { state with TrackState = Some { trackState with PlayerState = Playing(position, None) } },
                Cmd.none,
                None
            | PlaybackErrored -> notifyError "RequestPlay when trackState.PlayerState is PlaybackErrored"
        | None -> notifyError "RequestPlay when trackState is None"
    | Pause ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing (position, _) ->
                player.Pause()
                { state with TrackState = Some { trackState with PlayerState = Paused position } }, Cmd.none, None
            | _ -> notifyError "RequestPause when trackState.PlayerState not Playing"
        | None -> notifyError "RequestPause when trackState is None"
    | Stop ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _
            | Paused _ ->
                player.Stop()

                { state with TrackState = Some { trackState with PlayerState = Stopped START_POSITION } },
                Cmd.none,
                None
            | _ -> notifyError "RequestStop when trackState.PlayerState neither Playing nor Paused"
        | None -> notifyError "RequestStop when trackState is None"
    | ToggleMuted ->
        let newMuted = not state.Muted
        player.Mute <- newMuted
        { state with Muted = newMuted }, Cmd.none, None
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

            newState, Cmd.none, Some(ExternalMsg.NotifyPlaying(trackState.Track.Id, duration))
        | None -> notifyError "NotifyPlaying when trackState is None"
    | NotifyPaused ->
        match state.TrackState with
        | Some trackState -> state, Cmd.none, Some(ExternalMsg.NotifyPaused trackState.Track.Id)
        | None -> notifyError "NotifyPaused when trackState is None"
    | NotifyStopped ->
        match state.TrackState with
        | Some trackState ->
            let externalMsg =
                if trackState.PlayerState <> PlaybackErrored then
                    Some(ExternalMsg.NotifyStopped trackState.Track.Id)
                else
                    None

            state, Cmd.none, externalMsg
        | None -> notifyError "NotifyStopped when trackState is None"
    | NotifyEnded ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _ ->
                let externalMsg =
                    if trackState.HasNext then
                        Some(ExternalMsg.RequestNext(trackState.Track.Id, true))
                    else
                        Some(ExternalMsg.NotifyEnded trackState.Track.Id)

                { state with TrackState = Some { trackState with PlayerState = Ended } }, Cmd.none, externalMsg
            | _ -> notifyError "NotifyEnded when trackState.PlayerState not Playing"
        | None -> notifyError "NotifyEnded when trackState is None"
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
                None
            | _ -> notifyError "NotifyTimeChanged when trackState.PlayerState not Playing"
        | None -> notifyError "NotifyTimeChanged when trackState is None"
    | NotifyPlaybackErrored ->
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | AwaitingPlay _ ->
                { state with TrackState = Some { trackState with PlayerState = PlaybackErrored } },
                Cmd.none,
                Some(ExternalMsg.NotifyPlaybackErrored trackState.Track.Id)
            | _ -> notifyError "NotifyPlaybackErrored when trackState.PlayerState not AwaitingPlay"
        | None -> notifyError "NotifyPlaybackErrored when trackState is None"
