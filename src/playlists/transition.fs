module Aornota.Fap.Playlists.Transition

open Aornota.Fap.Domain
open Aornota.Fap.Playlists.Model
open Elmish

type ExternalMsg =
    | RequestPlay of track: TrackData * playlistName: string * hasPrevious: bool * hasNext: bool
    | NotifyError of string

type Msg =
    //| AddFiles of Track list
    | PlayTrack of TrackId
//| GetAny
//| GetNext
//| GetPrevious

// TODO-NMB: Reinstate real "init"...

let init = Temp.testState // TEMP-NMB

let private tryFindTrack (playlists: Playlist list) trackId =
    playlists
    |> List.choose (fun playlist ->
        match playlist.ItemsState with
        | NoItems -> None
        | Items (items, _, _) ->
            let matches =
                items.List
                |> List.choose (function
                    | Track trackData when trackData.Id = trackId -> Some trackData
                    | Track _
                    | Summary -> None)

            match matches with
            | [] -> None
            | h :: t -> Some(playlist, NonEmptyList<TrackData>.Create (h, t)))

let transition msg (Playlists playlists) : State * Cmd<Msg> * ExternalMsg option =
    let error text =
        Playlists playlists, Cmd.none, Some(NotifyError text)

    match msg with
    //| AddFiles files -> { state with SongList = Some files }, Cmd.none, None
    | PlayTrack trackId ->
        match tryFindTrack playlists.List trackId with
        // TEMP-NMB: To force errors...match tryFindTrack playlists.List (TrackId.Create()) with
        | [] -> error $"{nameof (PlayTrack)} {trackId} found no matches"
        | [ (playlist, tracks) ] ->
            match tracks.List with
            | [ trackData ] ->
                // TODO-NMB: Update state...
                // TODO-NMB: Ascertain hasPrevious | hasNext...
                Playlists playlists, Cmd.none, Some(RequestPlay(trackData, playlist.NameOrDefault, false, false))
            | _ -> error $"{nameof (PlayTrack)} {trackId} found multiple matches for playlist {playlist.Name}"
        | _ -> error $"{nameof (PlayTrack)} {trackId} found matches for multiple playlists"
(* | GetAny ->
        match state.SongList with
        | Some songs ->
            if songs.IsEmpty then
                state, Cmd.none, None
            else
                state, Cmd.ofMsg (PlaySong songs.Head), None
        | None -> state, Cmd.none, None *)
(* | GetNext ->
        match state.SongList with
        | Some songs ->
            if songs.IsEmpty then
                state, Cmd.none, None
            else if state.CurrentIndex + 1 >= songs.Length then
                state, Cmd.none, None
            else
                let song = songs.Item(state.CurrentIndex + 1)

                state, Cmd.ofMsg (PlaySong song), None
        | None -> state, Cmd.none, None *)
(* | GetPrevious ->
        match state.SongList with
        | Some songs ->
            if songs.IsEmpty then
                state, Cmd.none, None
            else if (state.CurrentIndex - 1) < 0 then
                state, Cmd.none, None
            else
                let song = songs.Item(state.CurrentIndex - 1)

                state, Cmd.ofMsg (PlaySong song), None
        | None -> state, Cmd.none, None *)
