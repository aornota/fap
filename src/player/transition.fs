module Aornota.Fap.Player.Transition

open Aornota.Fap.Domain
open Aornota.Fap.Player.Model
open Aornota.Fap.Player.Utilities
open LibVLCSharp.Shared
open System.IO

type ExternalMsg =
    | RequestPrevious of trackId: TrackId * play: bool
    | RequestNext of trackId: TrackId * play: bool
    | NotifyDuration of trackId: TrackId * duration: int64
    | NotifyPlaying of trackId: TrackId
    | NotifyPaused of trackId: TrackId
    | NotifyStopped of trackId: TrackId
    | NotifyError of string

type Msg =
    | TrackSelected of track: TrackData * hasPrevious: bool * hasNext: bool
    | PlayRequested of track: TrackData * hasPrevious: bool * hasNext: bool
    | TrackContextUpdated of trackId: TrackId * hasPrevious: bool * hasNext: bool
    | RequestPrevious
    | RequestNext
    | RequestPlay
    | RequestPause
    | RequestStop
    | RequestSeek of double
    | NotifyPlaying
    | NotifyPaused
    | NotifyStopped
    | NotifyEnded
    | NotifyPositionChanged of float32

let init: State = None

let transition msg (state: State) (player: MediaPlayer) =
    let isPlaying =
        function
        | Playing _ -> true
        | _ -> false

    let notifyError message =
        state, Some(NotifyError(sprintf "Player.transition -> %s" message))

    let play track =
        // TODO-NMB: Does this error if "file not found" (&c.)?...
        use media = getMediaFromlocal (Path.Combine(track.Folder, track.Name))
        player.Play media |> ignore
        player.Length

    let noChange = state, None

    match msg with
    | TrackSelected (track, hasPrevious, hasNext) ->
        let newState =
            match state with
            | Some trackState ->
                match trackState.PlayerState, trackState.Track.Id = track.Id with
                | NotLoaded, false ->
                    Some
                        { trackState with
                            Track = track
                            HasPrevious = hasPrevious
                            HasNext = hasNext }
                | _ -> state
            | None ->
                Some
                    { Track = track
                      PlayerState = NotLoaded
                      HasPrevious = hasPrevious
                      HasNext = hasNext }

        newState, None
    | PlayRequested (track, hasPrevious, hasNext) ->
        try
            let duration = play track

            Some
                { Track = { track with Duration = Some duration }
                  PlayerState = Playing(START_POSITION, None)
                  HasPrevious = hasPrevious
                  HasNext = hasNext },
            Some(NotifyDuration(track.Id, duration))
        with exn ->
            notifyError (sprintf "PlayRequested (%A): %s" track exn.Message)
    | TrackContextUpdated (trackId, hasPrevious, hasNext) ->
        match state with
        | Some trackState when trackState.Track.Id = trackId ->
            Some
                { trackState with
                    HasPrevious = hasPrevious
                    HasNext = hasNext },
            None
        | _ ->
            // TODO-NMB: Should this be an error?...
            noChange
    | RequestPrevious ->
        match state with
        | Some trackState when trackState.HasPrevious ->
            state, Some(ExternalMsg.RequestPrevious(trackState.Track.Id, isPlaying trackState.PlayerState))
        | Some _ -> notifyError "RequestPrevious when not trackState.HasPrevious"
        | None -> notifyError "RequestPrevious when trackState is None"
    | RequestNext ->
        match state with
        | Some trackState when trackState.HasNext ->
            state, Some(ExternalMsg.RequestNext(trackState.Track.Id, isPlaying trackState.PlayerState))
        | Some _ -> notifyError "RequestNext when not trackState.HasNext"
        | None -> notifyError "RequestNext when trackState is None"
    | RequestPlay ->
        match state with
        | Some trackState ->
            match trackState.PlayerState with
            | NotLoaded ->
                let track = trackState.Track

                try
                    let duration = play track

                    Some
                        { trackState with
                            Track = { track with Duration = Some duration }
                            PlayerState = Playing(START_POSITION, None) },
                    Some(NotifyDuration(track.Id, duration))
                with exn ->
                    notifyError (sprintf "RequestPlay (%A): %s" track exn.Message)
            | NotPlaying ->
                player.Play() |> ignore
                Some { trackState with PlayerState = Playing(START_POSITION, None) }, None
            | Playing _ -> notifyError "RequestPlay when trackState.PlayerState already Playing"
            | Paused position ->
                player.Pause()
                Some { trackState with PlayerState = Playing(position, None) }, None
        | None -> notifyError "RequestPlay when trackState is None"
    | RequestPause ->
        match state with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing (position, _) ->
                player.Pause()
                Some { trackState with PlayerState = Paused position }, None
            | _ -> notifyError "RequestPause when trackState.PlayerState not Playing"
        | None -> notifyError "RequestPause when trackState is None"
    | RequestStop ->
        match state with
        | Some trackState ->
            match trackState.PlayerState with
            | NotPlaying -> notifyError "RequestStop when trackState.PlayerState already NotPlaying"
            | _ ->
                player.Stop()
                Some { trackState with PlayerState = NotPlaying }, None
        | None -> notifyError "RequestStop when trackState is None"
    | RequestSeek sliderPosition ->
        match state with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing (_, lastPositionChanged) ->
                let position = sliderPosition / 100. |> float32

                if Some position <> lastPositionChanged then
                    player.Position <- position
                    Some { trackState with PlayerState = Playing(position, Some position) }, None
                else
                    noChange
            | _ -> notifyError "RequestSeek when trackState.PlayerState not Playing"
        | None -> notifyError "RequestSeek when trackState is None"
    | NotifyPlaying ->
        match state with
        | Some trackState -> state, Some(ExternalMsg.NotifyPlaying trackState.Track.Id)
        | None -> notifyError "NotifyPlaying when trackState is None"
    | NotifyPaused ->
        match state with
        | Some trackState -> state, Some(ExternalMsg.NotifyPaused trackState.Track.Id)
        | None -> notifyError "NotifyPaused when trackState is None"
    | NotifyStopped ->
        match state with
        | Some trackState -> state, Some(ExternalMsg.NotifyStopped trackState.Track.Id)
        | None -> notifyError "NotifyStopped when trackState is None"
    | NotifyEnded ->
        match state with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _ ->
                Some { trackState with PlayerState = NotPlaying },
                if trackState.HasNext then
                    Some(ExternalMsg.RequestNext(trackState.Track.Id, true))
                else
                    None
            | _ -> notifyError "NotifyEnded when trackState.PlayerState not Playing"
        | None -> notifyError "NotifyEnded when trackState is None"
    | NotifyPositionChanged position ->
        match state with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing _ -> Some { trackState with PlayerState = Playing(position, Some position) }, None
            | _ -> notifyError "NotifyTimeChanged when trackState.PlayerState not Playing"
        | None -> notifyError "NotifyTimeChanged when trackState is None"
