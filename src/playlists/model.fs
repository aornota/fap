module Aornota.Fap.Playlists.Model

open Aornota.Fap.Literals
open Aornota.Fap.Persistence
open Aornota.Fap.Utilities
open System
(* open System.IO *)

type TrackId =
    | TrackId of Guid

    static member Create() = TrackId(Guid.NewGuid())

type TrackData =
    { Id: TrackId
      Folder: string
      Name: string
      Duration: int64<millisecond> option }

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

type PlayerState =
    | NoMedia
    | AwaitingPlay
    | Playing of position: float32 * lastPositionChanged: float32 option
    | Paused of position: float32
    | Stopped of position: float32
    | Ended
    | PlaybackErrored

type TrackState =
    { Track: TrackData
      PlayerState: PlayerState
      HasPrevious: bool
      HasNext: bool }

type WritePlaylistRequestId =
    | WritePlaylistRequestId of Guid

    static member Create() = WritePlaylistRequestId(Guid.NewGuid())

type SeekRequestId =
    | SeekRequestId of Guid

    static member Create() = SeekRequestId(Guid.NewGuid())

type State =
    { Playlists: Playlist list
      SelectedPlaylistId: PlaylistId option
      Muted: bool
      Volume: int
      TrackState: TrackState option
      WritePlaylistRequests: (WritePlaylistRequestId * PlaylistId) list
      SeekRequests: SeekRequestId list }

[<Literal>]
let NEW_PLAYLIST = "new playlist"

[<Literal>]
let START_POSITION = 0f

let private playlistFile (PlaylistId guid) = $"{guid}.{fileExtension Playlist}"

let newPLaylist () =
    { Id = PlaylistId.Create()
      Name = NEW_PLAYLIST
      Items = [] }

let readPlaylist playlistId =
    async { return! read<Playlist> Playlist (playlistFile playlistId) }

let writePlaylist playlist =
    async { return! write Playlist (playlistFile playlist.Id) playlist }

let tracks playlist =
    playlist.Items
    |> List.choose (fun item ->
        match item with
        | Track trackData -> Some trackData
        | Summary -> None)

let findTrack playlists trackId =
    let matches =
        playlists
        |> List.choose (fun playlist ->
            let trackMatches =
                playlist.Items
                |> List.choose (fun item ->
                    match item with
                    | Track track when track.Id = trackId -> Some track
                    | _ -> None)

            match trackMatches with
            | _ :: _ -> Some(playlist, trackMatches)
            | [] -> None)

    match matches with
    | [ playlist, trackMatches ] ->
        match trackMatches with
        | [ trackData ] -> Ok(playlist, trackData)
        | [] -> Error $"no matches for {trackId} for {nameof (Playlist)} {playlist.Name}"
        | _ -> Error $"multiple matches for {trackId} for {nameof (Playlist)} {playlist.Name}"
    | [] -> Error $"no matches for {trackId}"
    | _ -> Error $"matches for {trackId} for multiple {nameof (Playlist)}s"

let iconVariant trackState =
    match trackState with
    | Some trackState ->
        match trackState.PlayerState with
        | NoMedia
        | Stopped _
        | Ended -> ICON_VARIANT_INACTIVE
        | AwaitingPlay _
        | Paused _ -> ICON_VARIANT_AWAITING
        | Playing _ -> ICON_VARIANT_ACTIVE
        | PlaybackErrored -> ICON_VARIANT_ACTIVE
    | None -> ICON_VARIANT_DISABLED

(*let tracksFromFiles files =
    files
    |> List.map FileInfo
    |> List.map (fun fi ->
        { Id = TrackId(Guid.NewGuid())
          Folder = fi.DirectoryName
          Name = fi.Name
          Duration = None })

let tracksFromFolder folder =
    let dottedFileExtensions = fileExtensions |> List.map (fun ext -> $".{ext}")

    match String.IsNullOrEmpty folder with
    | true -> []
    | false ->
        (DirectoryInfo folder).GetFiles()
        |> List.ofSeq
        |> List.filter (fun fi -> dottedFileExtensions |> List.contains fi.Extension)
        |> List.map (fun fi ->
            { Id = TrackId(Guid.NewGuid())
              Folder = fi.DirectoryName
              Name = fi.Name
              Duration = None }) *)
