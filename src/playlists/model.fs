module Aornota.Fap.Playlists.Model

open Aornota.Fap.Domain
open Aornota.Fap.Persistence
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

type WritePlaylistRequestId =
    | WritePlaylistRequestId of Guid

    static member Create() = WritePlaylistRequestId(Guid.NewGuid())

type State =
    { Playlists: Playlist list
      PlayerStatus: (TrackId * PlayerStatus) option
      WritePlaylistRequests: (WritePlaylistRequestId * PlaylistId) list }

let private playlistFile (PlaylistId guid) = $"{guid}.{fileExtension Playlist}"

let readPlaylist playlistId =
    async { return! read<Playlist> Playlist (playlistFile playlistId) }

let writePlaylist playlist =
    async { return! write Playlist (playlistFile playlist.Id) playlist }
