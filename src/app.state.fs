module Aornota.Fap.App.State

open Aornota.Fap
open System

type ErrorId =
    | ErrorId of Guid

    static member Create() = ErrorId(Guid.NewGuid())

type State =
    { PlayerState: Player.State.State
      PlaylistsState: Playlists.State.State
      Errors: (ErrorId * string) list }
