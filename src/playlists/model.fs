module Aornota.Fap.Playlists.Model

open Aornota.Fap
open Aornota.Fap.Literals.IconVariants
open Aornota.Fap.Persistence
open Aornota.Fap.Utilities
open System

[<Literal>]
let START_POSITION = 0f

type TrackId =
    | TrackId of Guid

    static member Create() = TrackId(Guid.NewGuid())

type TrackData =
    { Id: TrackId
      Folder: string
      Name: string
      Duration: int64<millisecond> option }

type SubTotalId =
    | SubTotalId of Guid

    static member Create() = SubTotalId(Guid.NewGuid())

type Item =
    | Track of TrackData
    | SubTotal of SubTotalId

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
      Previous: TrackData option
      Next: TrackData option }

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
      SeekRequests: SeekRequestId list
      SimulationState: Simulation.Model.State option }

let private playlistFile (PlaylistId guid) = $"{guid}.{fileExtension Playlist}"

let readPlaylist playlistId =
    async { return! read<Playlist> Playlist (playlistFile playlistId) }

let writePlaylist playlist =
    async { return! write Playlist (playlistFile playlist.Id) playlist }

let deletePlaylist playlistId =
    async { return! delete Playlist (playlistFile playlistId) }

let isTrackId trackId =
    function
    | Track track when track.Id = trackId -> true
    | _ -> false

let tracks playlist =
    playlist.Items
    |> List.choose (fun item ->
        match item with
        | Track trackData -> Some trackData
        | SubTotal _ -> None)

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
        | [] -> Error $"no matches for {trackId} for {playlist.Name}"
        | _ -> Error $"multiple matches for {trackId} for {playlist.Name}"
    | [] -> Error $"no matches for {trackId}"
    | _ -> Error $"matches for {trackId} for multiple {nameof (Playlist)}s"

let rec sanitize playlist =
    match playlist with
    | SubTotal _ :: t -> t |> sanitize
    | _ ->
        match playlist |> List.rev with
        | SubTotal _ :: t -> t |> List.rev |> sanitize
        | _ -> playlist

let iconVariant trackState =
    match trackState with
    | Some trackState ->
        match trackState.PlayerState with
        | NoMedia
        | Stopped _
        | Ended -> ICON_VARIANT_INACTIVE
        | AwaitingPlay
        | Paused _ -> ICON_VARIANT_AWAITING
        | Playing _ -> ICON_VARIANT_ACTIVE
        | PlaybackErrored -> ICON_VARIANT_ACTIVE
    | None -> ICON_VARIANT_DISABLED
