module Aornota.Fap.App.Model

open Aornota.Fap
open System

type ErrorId =
    | ErrorId of Guid

    static member Create() = ErrorId(Guid.NewGuid())

type State =
    { PlayerState: Player.Model.State
      PlaylistsState: Playlists.Model.State
      ShowingErrors: bool
      Errors: (ErrorId * DateTime * string) list }
