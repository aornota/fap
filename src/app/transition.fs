module Aornota.Fap.App.Transition

open Aornota.Fap
open Aornota.Fap.App.Model
open Aornota.Fap.Domain
open Aornota.Fap.Utilities
open Avalonia.Controls
open Avalonia.FuncUI.Hosts
open Elmish
open LibVLCSharp.Shared
open System

type Msg =
    | UpdateTitle of TrackData * playlistName: string
    | UpdateIcon
    | ToggleShowingErrors
    | AddError of string
    | RemoveError of ErrorId
    | ClearAllErrors
    (* | OpenFiles
    | OpenFolder
    | AfterSelectFolder of string
    | AfterSelectFiles of string array *)
    | SavePreferencesApp
    | SavePreferencesPlayer
    | DebounceSavePreferencesAppRequest of SavePreferencesRequestId
    | DebounceSavePreferencesPlayerRequest of SavePreferencesRequestId
    | NoOp
    | PlayerMsg of Player.Transition.Msg
    | PlayerPlaying
    | PlayerPaused
    | PlayerStopped
    | PlayerEnded
    | PlayerPositionChanged of float32
    | PlayerErrored
    | PlaylistsMsg of Playlists.Transition.Msg

[<Literal>]
let private DEBOUNCE_SAVE_PREFERENCES_REQUEST_DELAY = 250

let private makeError text = ErrorId.Create(), DateTime.UtcNow, text

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
                  Cmd.ofMsg (UpdateTitle(trackData, playlistName)) ]
        | Playlists.Transition.ExternalMsg.NotifyError text -> Cmd.ofMsg (AddError text)

let private handlePlayerExternal msg =
    match msg with
    | None -> Cmd.none
    | Some msg ->
        match msg with
        | Player.Transition.ExternalMsg.RequestPrevious (trackId, play) ->
            // TODO-NMB: Send message to Playlists...
            Cmd.none
        | Player.Transition.ExternalMsg.RequestNext (trackId, play) ->
            // TODO-NMB: Send message to Playlists...
            Cmd.none
        | Player.Transition.ExternalMsg.NotifyPlaying (trackID, duration) ->
            // TODO-NMB: Send message to Playlists...
            Cmd.batch [ Cmd.ofMsg UpdateIcon ]
        | Player.Transition.ExternalMsg.NotifyPaused trackID ->
            // TODO-NMB: Send message to Playlists...
            Cmd.batch [ Cmd.ofMsg UpdateIcon ]
        | Player.Transition.ExternalMsg.NotifyStopped trackID ->
            // TODO-NMB: Send message to Playlists...
            Cmd.batch [ Cmd.ofMsg UpdateIcon ]
        | Player.Transition.ExternalMsg.NotifyEnded trackID ->
            // TODO-NMB: Send message to Playlists...
            Cmd.batch [ Cmd.ofMsg UpdateIcon ]
        | Player.Transition.ExternalMsg.NotifyErrored trackID ->
            // TODO-NMB: Send message to Playlists...
            Cmd.batch [ Cmd.ofMsg UpdateIcon ]
        | Player.Transition.ExternalMsg.NotifyError text -> Cmd.ofMsg (AddError text)
        | Player.Transition.ExternalMsg.NotifyMutedToggled ->
            Cmd.batch [ Cmd.ofMsg UpdateIcon; Cmd.ofMsg SavePreferencesPlayer ]
        | Player.Transition.ExternalMsg.NotifyVolumeChanged -> Cmd.ofMsg SavePreferencesPlayer

let init preferences preferencesErrors =
    let errors = preferencesErrors |> List.map makeError

    { ShowingErrors = isDebug && errors.Length > 0
      Errors = errors
      LastNormalSize = preferences.NormalSize
      LastNormalLocation = preferences.NormalLocation
      LastWindowState = preferences.WindowState
      SavePreferencesAppRequestIds = []
      SavePreferencesPlayerRequestIds = []
      PlayerState = Player.Transition.init preferences.Muted preferences.Volume
      PlaylistsState = Playlists.Transition.init }

let transition msg (state: State) (window: HostWindow) (player: MediaPlayer) =
    let preferences () =
        let isNormal = window.WindowState = WindowState.Normal

        { NormalSize =
            if isNormal then
                window.Width, window.Height
            else
                state.LastNormalSize
          NormalLocation =
            if isNormal then
                window.Position.X, window.Position.Y
            else
                state.LastNormalLocation
          WindowState = window.WindowState
          Muted = state.PlayerState.Muted
          Volume = state.PlayerState.Volume }

    let handleWritePreferencesResult =
        function
        | Ok _ -> NoOp
        | Error error -> AddError error

    let noChange = state, Cmd.none

    match msg with
    | UpdateTitle (trackData, playlistName) ->
        window.Title <- $"{trackData.Name} | {playlistName} - {applicationNameAndVersion}"
        noChange
    | UpdateIcon ->
        let playerStatus =
            match state.PlayerState.TrackState with
            | Some trackState ->
                match trackState.PlayerState with
                | Player.Model.PlayerState.Playing _ -> Active
                | Player.Model.PlayerState.AwaitingPlay
                | Player.Model.PlayerState.Paused _ -> Awaiting
                | Player.Model.PlayerState.PlaybackErrored -> Errored
                | _ -> Inactive
            | None -> Inactive

        window.Icon <- applicationIcon playerStatus state.PlayerState.Muted
        noChange
    | ToggleShowingErrors -> { state with ShowingErrors = not state.ShowingErrors }, Cmd.none
    | AddError text -> { state with Errors = makeError text :: state.Errors }, Cmd.none
    | RemoveError errorId ->
        { state with
            Errors =
                state.Errors
                |> List.filter (fun (otherErrorId, _, _) -> otherErrorId <> errorId) },
        Cmd.none
    | ClearAllErrors -> { state with Errors = [] }, Cmd.none
    (* | OpenFiles ->
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
        state, Cmd.map PlaylistsMsg (Cmd.ofMsg (Playlists.Transition.Msg.AddFiles songs))
    | AfterSelectFiles paths ->
        let songs = populateSongs paths |> Array.toList
        state, Cmd.map PlaylistsMsg (Cmd.ofMsg (Playlists.Transition.Msg.AddFiles songs)) *)
    | SavePreferencesApp ->
        let savePreferencesRequestId = SavePreferencesRequestId.Create()

        let delay () =
            async {
                do! Async.Sleep DEBOUNCE_SAVE_PREFERENCES_REQUEST_DELAY
                return savePreferencesRequestId
            }

        { state with SavePreferencesAppRequestIds = savePreferencesRequestId :: state.SavePreferencesAppRequestIds },
        Cmd.OfAsync.perform delay () DebounceSavePreferencesAppRequest
    | SavePreferencesPlayer ->
        let savePreferencesRequestId = SavePreferencesRequestId.Create()

        let delay () =
            async {
                do! Async.Sleep DEBOUNCE_SAVE_PREFERENCES_REQUEST_DELAY
                return savePreferencesRequestId
            }

        { state with SavePreferencesPlayerRequestIds = savePreferencesRequestId :: state.SavePreferencesPlayerRequestIds },
        Cmd.OfAsync.perform delay () DebounceSavePreferencesPlayerRequest
    | DebounceSavePreferencesAppRequest savePreferencesRequestId ->
        let newSavePreferencesAppRequestIds =
            state.SavePreferencesAppRequestIds
            |> List.filter (fun otherSavePreferencesRequestId ->
                otherSavePreferencesRequestId <> savePreferencesRequestId)

        match newSavePreferencesAppRequestIds with
        | _ :: _ -> { state with SavePreferencesAppRequestIds = newSavePreferencesAppRequestIds }, Cmd.none
        | [] ->
            if
                (window.WindowState = WindowState.Normal
                 && ((window.Width, window.Height) <> state.LastNormalSize
                     || (window.Position.X, window.Position.Y) <> state.LastNormalLocation))
                || (window.WindowState <> WindowState.Minimized
                    && window.WindowState <> state.LastWindowState)
            then
                let preferences = preferences ()

                { state with
                    LastNormalSize = preferences.NormalSize
                    LastNormalLocation = preferences.NormalLocation
                    LastWindowState = preferences.WindowState
                    SavePreferencesAppRequestIds = [] },
                Cmd.OfAsync.perform writePreferences preferences handleWritePreferencesResult
            else
                { state with SavePreferencesAppRequestIds = [] }, Cmd.none
    | DebounceSavePreferencesPlayerRequest savePreferencesRequestId ->
        let newSavePreferencesPlayerRequestIds =
            state.SavePreferencesPlayerRequestIds
            |> List.filter (fun otherSavePreferencesRequestId ->
                otherSavePreferencesRequestId <> savePreferencesRequestId)

        match newSavePreferencesPlayerRequestIds with
        | _ :: _ -> { state with SavePreferencesPlayerRequestIds = newSavePreferencesPlayerRequestIds }, Cmd.none
        | [] ->
            let preferences = preferences ()

            { state with
                LastNormalSize = preferences.NormalSize
                LastNormalLocation = preferences.NormalLocation
                LastWindowState = preferences.WindowState
                SavePreferencesPlayerRequestIds = [] },
            Cmd.OfAsync.perform writePreferences preferences handleWritePreferencesResult
    | NoOp -> noChange
    | PlayerMsg playerMsg ->
        let newPlayerState, cmd, external =
            Player.Transition.transition playerMsg state.PlayerState player

        { state with PlayerState = newPlayerState }, Cmd.batch [ Cmd.map PlayerMsg cmd; handlePlayerExternal external ]
    | PlayerPlaying -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyPlaying)
    | PlayerPaused -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyPaused)
    | PlayerStopped -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyStopped)
    | PlayerEnded -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyEnded)
    | PlayerPositionChanged position ->
        state, Cmd.map PlayerMsg (Cmd.ofMsg (Player.Transition.Msg.NotifyPositionChanged position))
    | PlayerErrored -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyErrored)
    | PlaylistsMsg playlistsMsg ->
        let newPlaylistState, cmd, external =
            Playlists.Transition.transition playlistsMsg state.PlaylistsState

        { state with PlaylistsState = newPlaylistState },
        Cmd.batch [ Cmd.map PlaylistsMsg cmd; handlePlaylistsExternal external ]
