module Aornota.Fap.App.State

open Aornota.Fap

// TODO-NMB: Handle errors...

type State =
    { Title: string
      PlayerState: Player.State.State
      PlaylistsState: Playlists.State.State }
