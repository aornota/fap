module Aornota.Fap.Playlists.Model

open Aornota.Fap.Domain
open System

type Item =
    | Track of TrackData
    | Summary

type PlaylistId =
    | PlaylistId of Guid

    static member Create() = PlaylistId(Guid.NewGuid())

type Playlist =
    { Id: PlaylistId
      Name: string
      Items: Item list }

type State =
    { Playlists: Playlist list
      PlayerStatus: (TrackId * PlayerStatus) option }
