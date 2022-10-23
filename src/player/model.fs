module Aornota.Fap.Player.Model

open Aornota.Fap.Domain

type PlayerState =
    | NotLoaded
    | NotPlaying
    | Playing of position: float32 * lastPositionChanged: float32 option
    | Paused of position: float32

type TrackState =
    { Track: TrackData
      PlayerState: PlayerState
      HasPrevious: bool
      HasNext: bool }

type State = TrackState option

[<Literal>]
let START_POSITION = 0f
