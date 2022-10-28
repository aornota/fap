module Aornota.Fap.Playlists.Transition

open Aornota.Fap.Domain
open Aornota.Fap.Playlists.Model
open Aornota.Fap.Persistence
open Aornota.Fap.Utilities
open Elmish

type ExternalMsg =
    | RequestTrack of trackData: TrackData * hasPrevious: bool * hasNext: bool * play: bool
    | RequestWriteSession
    | RequestWritePreferences
    | NotifyError of string

type Msg =
    | ReadPlaylists of PlaylistId list * TrackId option
    | PlaylistRead of Playlist * PlaylistId list * TrackId option
    | ReadPlaylistError of ReadError * PlaylistId * PlaylistId list * TrackId option
    | WritePlaylist of PlaylistId
    | DebounceWritePlaylistRequest of WritePlaylistRequestId * PlaylistId
    | WritePlaylistError of string * PlaylistId
    | SelectPlaylist of PlaylistId
    | PlayTrack of TrackId
    | NoOp
    | NotifyRequestPrevious of TrackId * bool
    | NotifyRequestNext of TrackId * bool
    | NotifyPlaying of TrackId * int64<millisecond>
    | NotifyPaused of TrackId
    | NotifyStopped of TrackId
    | NotifyEnded of TrackId
    | NotifyPlaybackErrored of TrackId

[<Literal>]
let private DEBOUNCE_WRITE_PLAYLIST_REQUEST_DELAY = 250

let private playlist playlists playlistId =
    playlists |> List.tryFind (fun otherPlaylist -> otherPlaylist.Id = playlistId)

let private isTrackId trackId =
    function
    | Track track when track.Id = trackId -> true
    | _ -> false

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

let private defaultTrack playlists externalMsgs =
    match playlists with
    | playlist :: _ ->
        match tracks playlist with
        | trackData :: _ ->
            match hasPreviousAndOrNext playlist trackData.Id with
            | Ok (hasPrevious, hasNext) ->
                Some playlist.Id,
                Some(trackData.Id, Inactive),
                RequestTrack(trackData, hasPrevious, hasNext, false) :: externalMsgs
            | Error error -> None, None, NotifyError $"Playlists.transition -> {error}" :: externalMsgs
        | [] -> None, None, externalMsgs
    | [] -> None, None, externalMsgs

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

let init playlistIds lastTrackId = // TODO-NMB: Option to "auto-play" lastTrackId?...
    { Playlists = []
      SelectedPlaylistId = None
      PlayerStatus = None
      WritePlaylistRequests = [] },
    ReadPlaylists(playlistIds, lastTrackId)

// TODO-NMB: If change TrackId for PlayerStatus - or if set PlayerStatus to None (or Some(_, Errored)) - call (external) RequestWritePreferences...
// TODO-NMB: If add / remove / reorder Playlists, call (external) RequestWriteSession...
let transition msg state =
    let notifyError error =
        state, Cmd.none, [ NotifyError $"Playlists.transition -> {error}" ]

    let noChange = state, Cmd.none, []

    match msg with
    | ReadPlaylists (playlistIds, lastTrackId) ->
        match playlistIds with
        | playlistId :: playlistIds ->
            let read () =
                async { return! readPlaylist playlistId }

            let handleResult =
                function
                | Ok playlist -> PlaylistRead(playlist, playlistIds, lastTrackId)
                | Error error -> ReadPlaylistError(error, playlistId, playlistIds, lastTrackId)

            state, Cmd.OfAsync.perform read () handleResult, []
        | [] ->
            let lastTrack =
                match lastTrackId with
                | Some lastTrackId ->
                    match findTrack state.Playlists lastTrackId with
                    | Ok (playlist, trackData) ->
                        match hasPreviousAndOrNext playlist trackData.Id with
                        | Ok (hasPrevious, hasNext) ->
                            Some(playlist.Id, (trackData.Id, Inactive)),
                            [ RequestTrack(trackData, hasPrevious, hasNext, false) ]
                        | Error error -> None, [ NotifyError $"Playlists.transition -> {error}" ]
                    | Error error -> None, [ NotifyError $"Playlists.transition -> {error}" ]
                | None -> None, []

            let selectedPlaylistId, playerStatus, externalMsgs =
                match lastTrack with
                | (Some selectedPlaylistIdAndPlayerStatus, externalMsgs) ->
                    Some(fst selectedPlaylistIdAndPlayerStatus),
                    Some(snd selectedPlaylistIdAndPlayerStatus),
                    externalMsgs
                | (None, externalMsgs) -> defaultTrack state.Playlists externalMsgs

            { state with
                SelectedPlaylistId = selectedPlaylistId
                PlayerStatus = playerStatus },
            Cmd.none,
            RequestWritePreferences :: externalMsgs
    | PlaylistRead (playlist, playlistIds, lastTrackId) ->
        let newPlaylists = (playlist :: (state.Playlists |> List.rev)) |> List.rev

        { state with Playlists = newPlaylists },
        Cmd.ofMsg (ReadPlaylists(playlistIds, lastTrackId)),
        [ RequestWriteSession ]
    | ReadPlaylistError (readError, PlaylistId guid, playlistIds, lastTrackId) ->
        state,
        Cmd.ofMsg (ReadPlaylists(playlistIds, lastTrackId)),
        [ NotifyError $"Playlists.transition -> Unable to read playlist ({guid}): {readErrorText readError}" ]
    | WritePlaylist playlistId ->
        let writePlaylistRequest = WritePlaylistRequestId.Create(), playlistId

        let delay () =
            async {
                do! Async.Sleep DEBOUNCE_WRITE_PLAYLIST_REQUEST_DELAY
                return writePlaylistRequest
            }

        { state with WritePlaylistRequests = writePlaylistRequest :: state.WritePlaylistRequests },
        Cmd.OfAsync.perform delay () DebounceWritePlaylistRequest,
        []
    | DebounceWritePlaylistRequest (writePlaylistRequestId, playlistId) ->
        let newWritePlaylistPlayerRequests =
            state.WritePlaylistRequests
            |> List.filter (fun (otherWritePlaylistRequestId, _) ->
                otherWritePlaylistRequestId <> writePlaylistRequestId)

        match
            newWritePlaylistPlayerRequests
            |> List.filter (fun (_, otherPlaylistId) -> otherPlaylistId = playlistId)
        with
        | _ :: _ -> { state with WritePlaylistRequests = newWritePlaylistPlayerRequests }, Cmd.none, []
        | [] ->
            let writePlaylist playlistId =
                async {
                    match playlist state.Playlists playlistId with
                    | Some playlist -> return! writePlaylist playlist
                    | None -> return Error "Playlist not found"
                }

            let handleResult =
                function
                | Ok _ -> NoOp
                | Error error -> WritePlaylistError(error, playlistId)

            { state with WritePlaylistRequests = newWritePlaylistPlayerRequests },
            Cmd.OfAsync.perform writePlaylist playlistId handleResult,
            []
    | WritePlaylistError (error, PlaylistId guid) -> notifyError $"Unable to write playlist ({guid}): {error}"
    | SelectPlaylist playlistId -> { state with SelectedPlaylistId = Some playlistId }, Cmd.none, []
    | PlayTrack trackId ->
        match findTrack state.Playlists trackId with
        | Ok (playlist, trackData) ->
            match hasPreviousAndOrNext playlist trackData.Id with
            | Ok (hasPrevious, hasNext) ->
                { state with PlayerStatus = Some(trackData.Id, Awaiting) },
                Cmd.none,
                [ RequestTrack(trackData, hasPrevious, hasNext, true); RequestWritePreferences ]
            | Error error -> notifyError $"{nameof (PlayTrack)}: {error}"
        | Error error -> notifyError $"{nameof (PlayTrack)}: {error}"
    | NoOp -> noChange
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
                    [ RequestTrack(previous, hasPrevious, hasNext, play); RequestWritePreferences ]
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
                    [ RequestTrack(next, hasPrevious, hasNext, play); RequestWritePreferences ]
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
                        Cmd.ofMsg (WritePlaylist newPlaylist.Id),
                        []
                    | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}"
                | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}"
            else
                { state with PlayerStatus = Some(trackId, Active) }, Cmd.none, []
        | Error error -> notifyError $"{nameof (NotifyPlaying)}: {error}"
    | NotifyPaused trackId -> { state with PlayerStatus = Some(trackId, Awaiting) }, Cmd.none, []
    | NotifyStopped trackId
    | NotifyEnded trackId -> { state with PlayerStatus = Some(trackId, Inactive) }, Cmd.none, []
    | NotifyPlaybackErrored trackId ->
        { state with PlayerStatus = Some(trackId, Errored) }, Cmd.none, [ RequestWritePreferences ]
