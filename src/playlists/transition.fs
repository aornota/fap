module Aornota.Fap.Playlists.Transition

open Aornota.Fap.Domain
open Aornota.Fap.Playlists.Model
open Aornota.Fap.Utilities
open Elmish

type ExternalMsg =
    | RequestTrack of trackData: TrackData * playlistName: string * hasPrevious: bool * hasNext: bool * play: bool
    | NotifyError of string

type Msg =
    | PlayTrack of TrackId
    | NotifyRequestPrevious of TrackId * bool
    | NotifyRequestNext of TrackId * bool
    | NotifyPlaying of TrackId * int64<millisecond>
    | NotifyPaused of TrackId
    | NotifyStopped of TrackId
    | NotifyEnded of TrackId
    | NotifyPlaybackErrored of TrackId

let private isTrackId trackId =
    function
    | Track track when track.Id = trackId -> true
    | _ -> false

let private findTrack playlists trackId =
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

let tracks playlist =
    playlist.Items
    |> List.choose (fun item ->
        match item with
        | Track trackData -> Some trackData
        | Summary -> None)

let private previousAndOrNext playlist trackId =
    let tracks = tracks playlist

    match tracks |> List.tryFindIndex (fun trackData -> trackData.Id = trackId) with
    | Some index ->
        let previous = if index > 0 then Some tracks[index - 1] else None

        let next =
            if index < tracks.Length - 1 then
                Some tracks[index + 1]
            else
                None

        Ok(previous, next)
    | None -> Error $"no matches for {trackId} for {nameof (Playlist)} {playlist.Name}"

let private hasPreviousAndOrNext playlist trackId =
    match previousAndOrNext playlist trackId with
    | Ok (Some _, Some _) -> Ok(true, true)
    | Ok (Some _, None) -> Ok(true, false)
    | Ok (None, Some _) -> Ok(false, true)
    | Ok (None, None) -> Ok(false, false)
    | Error error -> Error error

let private updatePlaylist playlist (trackData: TrackData) =
    let mutable updated = 0

    let newItems =
        playlist.Items
        |> List.map (fun item ->
            if isTrackId trackData.Id item then
                updated <- updated + 1
                Track trackData
            else
                item)

    if updated = 1 then
        Ok { playlist with Items = newItems }
    else if updated = 0 then
        Error $"no matches for {trackData.Id} for {nameof (Playlist)} {playlist.Name}"
    else
        Error $"multiple matches for {trackData.Id} for {nameof (Playlist)} {playlist.Name}"

let private updatePlaylists (playlists: Playlist list) (playlist: Playlist) =
    let mutable updated = 0

    let newPlaylists =
        playlists
        |> List.map (fun otherPlaylist ->
            if otherPlaylist.Id = playlist.Id then
                updated <- updated + 1
                playlist
            else
                otherPlaylist)

    if updated = 1 then
        Ok newPlaylists
    else if updated = 0 then
        Error $"no matches for {playlist.Id} for {nameof (Playlist)}s"
    else
        Error $"multiple matches for {playlist.Id} for {nameof (Playlist)}s"

// TODO-NMB: Pass in "auto-play on load"? "last requested TrackId"?...
let init (playlists: Playlist list) =
    let playerStatus, externalMsg =
        match playlists with
        | playlist :: _ ->
            match tracks playlist with
            | trackData :: _ ->
                match hasPreviousAndOrNext playlist trackData.Id with
                | Ok (hasPrevious, hasNext) ->
                    Some(trackData.Id, Inactive),
                    [ RequestTrack(trackData, playlist.Name, hasPrevious, hasNext, false) ]
                | Error error -> None, [ NotifyError $"Playlists.init -> {error}" ]
            | [] -> None, []
        | [] -> None, []

    { Playlists = playlists
      PlayerStatus = playerStatus },
    externalMsg

let transition msg state =
    let notifyError error =
        state, Cmd.none, [ NotifyError $"Playlists.transition -> {error}" ]

    let noChange = state, Cmd.none, []

    match msg with
    | PlayTrack trackId ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) ->
            match hasPreviousAndOrNext playlist trackId with
            | Ok (hasPrevious, hasNext) ->
                { state with PlayerStatus = Some(trackId, Awaiting) },
                Cmd.none,
                [ RequestTrack(trackData, playlist.Name, hasPrevious, hasNext, true) ]
            | Error error -> notifyError $"{nameof (PlayTrack)}: {error}"
        | Error error -> notifyError $"{nameof (PlayTrack)}: {error}"
    | NotifyRequestPrevious (trackId, play) ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) ->
            match previousAndOrNext playlist trackId with
            | Ok (Some previous, _) ->
                match hasPreviousAndOrNext playlist previous.Id with
                | Ok (hasPrevious, hasNext) ->
                    let playerStatus = if play then Awaiting else Inactive

                    { state with PlayerStatus = Some(previous.Id, playerStatus) },
                    Cmd.none,
                    [ RequestTrack(previous, playlist.Name, hasPrevious, hasNext, play) ]
                | Error error -> notifyError $"{nameof (NotifyRequestPrevious)}: {error}"
            | Ok (None, _) ->
                notifyError
                    $"{nameof (NotifyRequestPrevious)}: no previous track for {trackId} for {nameof (Playlist)} {playlist.Name}"
            | Error error -> notifyError $"{nameof (NotifyRequestPrevious)}: {error}"
        | Error error -> notifyError $"{nameof (NotifyRequestPrevious)}: {error}"
    | NotifyRequestNext (trackId, play) ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) ->
            match previousAndOrNext playlist trackId with
            | Ok (_, Some next) ->
                match hasPreviousAndOrNext playlist next.Id with
                | Ok (hasPrevious, hasNext) ->
                    let playerStatus = if play then Awaiting else Inactive

                    { state with PlayerStatus = Some(next.Id, playerStatus) },
                    Cmd.none,
                    [ RequestTrack(next, playlist.Name, hasPrevious, hasNext, play) ]
                | Error error -> notifyError $"{nameof (NotifyRequestNext)}: {error}"
            | Ok (_, None) ->
                notifyError
                    $"{nameof (NotifyRequestNext)}: no next track for {trackId} for {nameof (Playlist)} {playlist.Name}"
            | Error error -> notifyError $"{nameof (NotifyRequestPrevious)}: {error}"
        | Error error -> notifyError $"{nameof (NotifyRequestPrevious)}: {error}"
    | NotifyPlaying (trackId, duration) ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) ->
            if trackData.Duration <> Some duration then
                match updatePlaylist playlist { trackData with Duration = Some duration } with
                | Ok newPlaylist ->
                    match updatePlaylists state.Playlists newPlaylist with
                    | Ok newPlaylists ->
                        { state with
                            Playlists = newPlaylists
                            PlayerStatus = Some(trackId, Active) },
                        Cmd.none,
                        []
                    | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}"
                | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}"
            else
                { state with PlayerStatus = Some(trackId, Active) }, Cmd.none, []
        | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}"
    | NotifyPaused trackId -> { state with PlayerStatus = Some(trackId, Awaiting) }, Cmd.none, []
    | NotifyStopped trackId
    | NotifyEnded trackId -> { state with PlayerStatus = Some(trackId, Inactive) }, Cmd.none, []
    | NotifyPlaybackErrored trackId -> { state with PlayerStatus = Some(trackId, Errored) }, Cmd.none, []
