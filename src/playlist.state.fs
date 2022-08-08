[<RequireQualifiedAccess>]
module Aornota.Fap.Playlist.State

open Aornota.Fap.Domain

type State =
    { SongList: SongRecord list option
      CurrentIndex: int }
