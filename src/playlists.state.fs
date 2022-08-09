module Aornota.Fap.Playlists.State

open Aornota.Fap.Domain
open System

type NonEmptyList<'a> =
    { Head: 'a
      Tail: 'a list }

    static member Create(head, tail) = { Head = head; Tail = tail }
    member x.List = x.Head :: x.Tail

type Item =
    | Track of Track
    | Summary

type PlaylistId = PlaylistId of Guid

type ItemsState =
    | NoItems
    | Items of NonEmptyList<Item> * selected: TrackId * isPlaying: TrackId option

type Playlist =
    { Id: PlaylistId
      Path: string
      Name: string
      ItemsState: ItemsState }

type State =
    | NoPlaylists
    | Playlists of NonEmptyList<Playlist> * selected: PlaylistId
