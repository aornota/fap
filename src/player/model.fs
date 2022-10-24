module Aornota.Fap.Player.Model

open Aornota.Fap.Domain
open System

type PlayerState =
    | NoMedia
    | AwaitingPlay
    | Playing of position: float32 * lastPositionChanged: float32 option
    | Paused of position: float32
    | Stopped
    | Ended
    | PlaybackErrored

type TrackState =
    { Track: TrackData
      PlaylistName: string
      PlayerState: PlayerState
      HasPrevious: bool
      HasNext: bool }

type SeekRequestId =
    | SeekRequestId of Guid

    static member Create() = SeekRequestId(Guid.NewGuid())

type State =
    { TrackState: TrackState option
      SeekRequests: SeekRequestId list }

[<Literal>]
let START_POSITION = 0f
