[<RequireQualifiedAccess>]
module Aornota.Fap.Playlist.Transition

open Aornota.Fap.Domain
open Aornota.Fap.Playlist
open Elmish

type ExternalMsg = PlaySong of index: int * song: SongRecord

type Msg =
    | GetAny
    | GetNext
    | GetPrevious
    | AddFiles of SongRecord list
    | PlaySong of SongRecord

let init: State.State = { SongList = None; CurrentIndex = 0 }

let private tryFindSong (songlist: SongRecord list option) (song: SongRecord) =
    match songlist with
    | Some songlist ->
        match songlist |> List.tryFindIndex (fun (sng: SongRecord) -> sng.id = song.id) with
        | Some index -> Some index
        | None -> None
    | None -> None

let update (msg: Msg) (state: State.State) : State.State * Cmd<Msg> * ExternalMsg option =
    match msg with
    | AddFiles files -> { state with SongList = Some files }, Cmd.none, None
    | PlaySong song ->
        let index = tryFindSong state.SongList song

        match index with
        | Some index -> { state with CurrentIndex = index }, Cmd.none, Some(ExternalMsg.PlaySong(index, song))
        | None -> state, Cmd.none, None
    | GetAny ->
        match state.SongList with
        | Some songs ->
            if songs.IsEmpty then
                state, Cmd.none, None
            else
                state, Cmd.ofMsg (PlaySong songs.Head), None
        | None -> state, Cmd.none, None
    | GetNext ->
        match state.SongList with
        | Some songs ->
            if songs.IsEmpty then
                state, Cmd.none, None
            else if state.CurrentIndex + 1 >= songs.Length then
                state, Cmd.none, None
            else
                let song = songs.Item(state.CurrentIndex + 1)

                state, Cmd.ofMsg (PlaySong song), None
        | None -> state, Cmd.none, None
    | GetPrevious ->
        match state.SongList with
        | Some songs ->
            if songs.IsEmpty then
                state, Cmd.none, None
            else if (state.CurrentIndex - 1) < 0 then
                state, Cmd.none, None
            else
                let song = songs.Item(state.CurrentIndex - 1)

                state, Cmd.ofMsg (PlaySong song), None
        | None -> state, Cmd.none, None
