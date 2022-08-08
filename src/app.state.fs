[<RequireQualifiedAccess>]
module Aornota.Fap.App.State

open Aornota.Fap

type State =
    { Title: string
      PlayerState: Player.State.State
      PlaylistState: Playlist.State.State }
