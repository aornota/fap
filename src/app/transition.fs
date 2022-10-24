module Aornota.Fap.App.Transition

open Aornota.Fap
open Aornota.Fap.App.Model
open Aornota.Fap.Domain
open Avalonia
open Avalonia.FuncUI.Hosts
open Elmish
open LibVLCSharp.Shared

[<Literal>]
let AppName = "fap"

type Msg =
    | PlayerMsg of Player.Transition.Msg
    | PlaylistsMsg of Playlists.Transition.Msg
    | SetTitle of TrackData * playlistName: string
    | AddError of string
    | DismissError of ErrorId
    | OpenFiles
    | OpenFolder
    | AfterSelectFolder of string
    | AfterSelectFiles of string array
    | Playing
    | Paused
    | Stopped
    | Ended
    | PositionChanged of float32
    | PlaybackErrored

let private handlePlaylistsExternal msg =
    match msg with
    | None -> Cmd.none
    | Some msg ->
        match msg with
        | Playlists.Transition.ExternalMsg.RequestPlay (trackData, playlistName, hasPrevious, hasNext) ->
            Cmd.batch
                [ Cmd.ofMsg (
                      PlayerMsg(Player.Transition.Msg.PlayRequested(trackData, playlistName, hasPrevious, hasNext))
                  )
                  Cmd.ofMsg (SetTitle(trackData, playlistName)) ]
        | Playlists.Transition.ExternalMsg.NotifyError text -> Cmd.ofMsg (AddError text)

let private handlePlayerExternal msg =
    match msg with
    | None -> Cmd.none
    | Some msg ->
        match msg with
        | Player.Transition.ExternalMsg.RequestPrevious (trackId, play) ->
            // TODO-NMB...Cmd.ofMsg (PlaylistsMsg(Playlists.Transition.Msg.GetPrevious))
            Cmd.none
        | Player.Transition.ExternalMsg.RequestNext (trackId, play) ->
            // TODO-NMB...Cmd.ofMsg (PlaylistsMsg(Playlists.Transition.Msg.GetNext))
            Cmd.none
        | Player.Transition.ExternalMsg.NotifyPlaying (trackID, duration) ->
            // TODO-NMB...
            Cmd.none
        | Player.Transition.ExternalMsg.NotifyPaused trackID ->
            // TODO-NMB...
            Cmd.none
        | Player.Transition.ExternalMsg.NotifyStopped trackID ->
            // TODO-NMB...
            Cmd.none
        | Player.Transition.ExternalMsg.NotifyEnded trackID ->
            // TODO-NMB...
            Cmd.none
        | Player.Transition.ExternalMsg.NotifyPlaybackErrored trackID ->
            // TODO-NMB...
            Cmd.none
        | Player.Transition.ExternalMsg.NotifyError text -> Cmd.ofMsg (AddError text)

let init =
    { PlayerState = Player.Transition.init
      PlaylistsState = Playlists.Transition.init
      Errors = [] }

let transition msg (state: State) (window: HostWindow) (player: MediaPlayer) =
    match msg with
    | PlayerMsg playerMsg ->
        let newPlayerState, cmd, external =
            Player.Transition.transition playerMsg state.PlayerState player

        { state with PlayerState = newPlayerState }, Cmd.batch [ Cmd.map PlayerMsg cmd; handlePlayerExternal external ]
    | PlaylistsMsg playlistsMsg ->
        let newPlaylistState, cmd, external =
            Playlists.Transition.transition playlistsMsg state.PlaylistsState

        { state with PlaylistsState = newPlaylistState },
        Cmd.batch [ Cmd.map PlaylistsMsg cmd; handlePlaylistsExternal external ]
    | AddError text -> { state with Errors = (ErrorId.Create(), text) :: state.Errors }, Cmd.none
    | DismissError errorId ->
        { state with Errors = state.Errors |> List.filter (fun error -> fst error <> errorId) }, Cmd.none
    | SetTitle (trackData, playlistName) ->
        window.Title <- $"{trackData.Name} | {playlistName} - {AppName}"
        state, Cmd.none
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
        (* TODO-NMB...let songs = populateFromDirectory path |> Array.toList
        state, Cmd.map PlaylistsMsg (Cmd.ofMsg (Playlists.Transition.Msg.AddFiles songs)) *)
        state, Cmd.none
    | AfterSelectFiles paths ->
        (* TODO-NMB...let songs = populateSongs paths |> Array.toList
        state, Cmd.map PlaylistsMsg (Cmd.ofMsg (Playlists.Transition.Msg.AddFiles songs)) *)
        state, Cmd.none
    | Playing -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyPlaying)
    | Paused -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyPaused)
    | Stopped -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyStopped)
    | Ended -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyEnded)
    | PositionChanged position ->
        state, Cmd.map PlayerMsg (Cmd.ofMsg (Player.Transition.Msg.NotifyPositionChanged position))
    | PlaybackErrored -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyPlaybackErrored)
