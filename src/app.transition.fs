[<RequireQualifiedAccess>]
module Aornota.Fap.App.Transition

open Aornota.Fap
open Aornota.Fap.Domain
open Avalonia
open Avalonia.FuncUI.Hosts
open Elmish
open LibVLCSharp.Shared

[<Literal>]
let AppName = "fap"

type Msg =
    | PlayerMsg of Player.Transition.Msg
    | PlaylistMsg of Playlist.Transition.Msg
    | SetTitle of string
    | OpenFiles
    | OpenFolder
    | AfterSelectFolder of string
    | AfterSelectFiles of string array
    | Playing
    | Paused
    | Stopped
    | Ended
    | TimeChanged of int64
    | ChapterChanged of int
    | LengthChanged of int64

let private handlePlaylistExternal msg =
    match msg with
    | None -> Cmd.none
    | Some msg ->
        match msg with
        | Playlist.Transition.ExternalMsg.PlaySong (int, song) ->
            Cmd.batch
                [ Cmd.ofMsg (PlayerMsg(Player.Transition.Msg.Play song))
                  Cmd.ofMsg (SetTitle song.name) ]

let private handlePlayerExternal msg =
    match msg with
    | None -> Cmd.none
    | Some msg ->
        match msg with
        | Player.Transition.ExternalMsg.Play -> Cmd.ofMsg (PlaylistMsg(Playlist.Transition.Msg.GetAny))
        | Player.Transition.ExternalMsg.Next -> Cmd.ofMsg (PlaylistMsg(Playlist.Transition.Msg.GetNext))
        | Player.Transition.ExternalMsg.Previous -> Cmd.ofMsg (PlaylistMsg(Playlist.Transition.Msg.GetPrevious))

let init: State.State =
    { Title = AppName
      PlayerState = Player.Transition.init
      PlaylistState = Playlist.Transition.init }

let transition (msg: Msg) (state: State.State) (window: HostWindow) (player: MediaPlayer) =
    match msg with
    | PlayerMsg playermsg ->
        let s, cmd, external = Player.Transition.update playermsg state.PlayerState player
        let handled = handlePlayerExternal external
        let mapped = Cmd.map PlayerMsg cmd
        let batch = Cmd.batch [ mapped; handled ]
        { state with PlayerState = s }, batch
    | PlaylistMsg playlistmsg ->
        let s, cmd, external = Playlist.Transition.update playlistmsg state.PlaylistState
        let mapped = Cmd.map PlaylistMsg cmd
        let handled = handlePlaylistExternal external
        let batch = Cmd.batch [ mapped; handled ]
        { state with PlaylistState = s }, batch
    | SetTitle title ->
        window.Title <- $"{AppName} - {title}"
        { state with Title = title }, Cmd.none
    | OpenFiles ->
        let dialog = Dialogs.getFilesDialog None

        let showDialog window =
            dialog.ShowAsync(window) |> Async.AwaitTask

        state, Cmd.OfAsync.perform showDialog window AfterSelectFiles
    | OpenFolder ->
        let dialog = Dialogs.getFolderDialog ()

        let showDialog window =
            dialog.ShowAsync(window) |> Async.AwaitTask

        state, Cmd.OfAsync.perform showDialog window AfterSelectFolder
    | AfterSelectFolder path ->
        let songs = populateFromDirectory path |> Array.toList
        state, Cmd.map PlaylistMsg (Cmd.ofMsg (Playlist.Transition.Msg.AddFiles songs))
    | AfterSelectFiles paths ->
        let songs = populateSongs paths |> Array.toList

        state, Cmd.map PlaylistMsg (Cmd.ofMsg (Playlist.Transition.Msg.AddFiles songs))
    | Playing -> state, Cmd.none // TODO-NMB: Is this necessary?...
    | Paused -> state, Cmd.none // TODO-NMB: Is this necessary?...
    | Stopped -> state, Cmd.none // TODO-NMB: Is this necessary?...
    | Ended -> state, Cmd.map PlaylistMsg (Cmd.ofMsg (Playlist.Transition.Msg.GetNext))
    | TimeChanged time -> state, Cmd.map PlayerMsg (Cmd.ofMsg (Player.Transition.Msg.SetPos time))
    | ChapterChanged chapter -> state, Cmd.none // TODO-NMB: Is this necessary?...
    | LengthChanged length -> state, Cmd.none // TODO-NMB: Is this necessary?...
