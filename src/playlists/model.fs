module Aornota.Fap.Playlists.Model

open Aornota.Fap.Domain
open System

type NonEmptyList<'a> =
    { Head: 'a
      Tail: 'a list }

    static member Create(head) = NonEmptyList<'a>.Create (head, [])
    static member Create(head, tail) = { Head = head; Tail = tail }
    member x.List = x.Head :: x.Tail

type Item =
    | Track of TrackData
    | Summary

type PlaylistId =
    | PlaylistId of Guid

    static member Create() = PlaylistId(Guid.NewGuid())

type ItemsState =
    | NoItems
    | Items of NonEmptyList<Item> * selected: TrackId * isPlaying: TrackId option

type Playlist =
    { Id: PlaylistId
      Name: string option
      ItemsState: ItemsState }

    member x.NameOrDefault = x.Name |> Option.defaultValue "<unnamed>"

type State = Playlists of NonEmptyList<Playlist>
