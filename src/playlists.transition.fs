module Aornota.Fap.Playlists.Transition

open Aornota.Fap.Domain
open Aornota.Fap.Playlists.State
open Elmish

type ExternalMsg =
    | PlaySong of index: int * song: Track
    | Error of string

type Msg =
    //| AddFiles of Track list
    | PlayTrack of TrackId
//| GetAny
//| GetNext
//| GetPrevious

// TODO-NMB: Fake "init" with test data...

let init = NoPlaylists

let private tryFindTrack (playlists: Playlist list) trackId =
    playlists
    |> List.choose (fun playlist ->
        match playlist.ItemsState with
        | NoItems -> None
        | Items (items, _, _) ->
            let matches =
                items.List
                |> List.choose (function
                    | Track track when track.Id = trackId -> Some track
                    | Track _
                    | Summary -> None)

            match matches with
            | [] -> None
            | h :: t -> Some(playlist, NonEmptyList<Track>.Create (h, t)))

let update msg state : State * Cmd<Msg> * ExternalMsg option =
    let error text = state, Cmd.none, Some(Error text)

    match msg with
    //| AddFiles files -> { state with SongList = Some files }, Cmd.none, None
    | PlayTrack trackId ->
        match state with
        | NoPlaylists -> error $"{nameof (PlayTrack)} {trackId} when {nameof (NoPlaylists)}"
        | Playlists (playlists, selected) ->
            match tryFindTrack playlists.List trackId with
            | [] -> error $"{nameof (PlayTrack)} {trackId} found no matches"
            | [ (playlist, tracks) ] ->
                match tracks.List with
                | [ track ] ->
                    // TODO-NMB: Update state...
                    state, Cmd.none, Some(PlaySong(0, track))
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
