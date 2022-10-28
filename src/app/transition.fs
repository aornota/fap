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
    | UpdateTitle
    | UpdateIcon
    | ToggleShowingErrors
    | AddError of string
    | ClearError of ErrorId
    | ClearAllErrors
    | WriteSession
    | DebounceWriteSessionRequest of WriteSessionRequestId
    | WritePreferences of WritePreferencesRequestSource
    | DebounceWritePreferencesRequest of WritePreferencesRequestId * WritePreferencesRequestSource
    (* | OpenFiles
    | OpenFolder
    | AfterSelectFolder of string
    | AfterSelectFiles of string array *)
    | NoOp
    | PlaylistsMsg of Playlists.Transition.Msg
    | PlayerMsg of Player.Transition.Msg
    | PlayerPlaying
    | PlayerPaused
    | PlayerStopped
    | PlayerEnded
    | PlayerPositionChanged of float32
    | PlayerErrored

[<Literal>]
let private DEBOUNCE_WRITE_SESSION_REQUEST_DELAY = 250

[<Literal>]
let private DEBOUNCE_WRITE_PREFERENCES_REQUEST_DELAY = 250

let private makeError error =
    ErrorId.Create(), DateTime.UtcNow, error

let private handlePlaylistsExternal msg =
    match msg with
    | Playlists.Transition.ExternalMsg.RequestTrack (trackData, hasPrevious, hasNext, play) ->
        Cmd.ofMsg (PlayerMsg(Player.Transition.Msg.NotifyTrackRequested(trackData, hasPrevious, hasNext, play)))
    | Playlists.Transition.ExternalMsg.NotifyPlaylistsChanged -> Cmd.ofMsg WriteSession
    | Playlists.Transition.ExternalMsg.NotifyPlayerStatusChanged ->
        Cmd.batch [ Cmd.ofMsg UpdateTitle; Cmd.ofMsg (WritePreferences Playlists) ]
    | Playlists.Transition.ExternalMsg.NotifyError text -> Cmd.ofMsg (AddError text)

let private handlePlayerExternal msg =
    match msg with
    | Player.Transition.ExternalMsg.RequestPrevious (trackId, play) ->
        Cmd.ofMsg (PlaylistsMsg(Playlists.Transition.Msg.NotifyRequestPrevious(trackId, play)))
    | Player.Transition.ExternalMsg.RequestNext (trackId, play) ->
        Cmd.ofMsg (PlaylistsMsg(Playlists.Transition.Msg.NotifyRequestNext(trackId, play)))
    | Player.Transition.ExternalMsg.NotifyPlaying (trackId, duration) ->
        Cmd.batch
            [ Cmd.ofMsg (PlaylistsMsg(Playlists.Transition.Msg.NotifyPlaying(trackId, duration)))
              Cmd.ofMsg UpdateIcon ]
    | Player.Transition.ExternalMsg.NotifyPaused trackId ->
        Cmd.batch
            [ Cmd.ofMsg (PlaylistsMsg(Playlists.Transition.Msg.NotifyPaused trackId))
              Cmd.ofMsg UpdateIcon ]
    | Player.Transition.ExternalMsg.NotifyStopped trackId ->
        Cmd.batch
            [ Cmd.ofMsg (PlaylistsMsg(Playlists.Transition.Msg.NotifyStopped trackId))
              Cmd.ofMsg UpdateIcon ]
    | Player.Transition.ExternalMsg.NotifyEnded trackId ->
        Cmd.batch
            [ Cmd.ofMsg (PlaylistsMsg(Playlists.Transition.Msg.NotifyEnded trackId))
              Cmd.ofMsg UpdateIcon ]
    | Player.Transition.ExternalMsg.NotifyPlaybackErrored trackId ->
        Cmd.batch
            [ Cmd.ofMsg (PlaylistsMsg(Playlists.Transition.Msg.NotifyPlaybackErrored trackId))
              Cmd.ofMsg UpdateIcon ]
    | Player.Transition.ExternalMsg.NotifyMutedToggled ->
        Cmd.batch [ Cmd.ofMsg UpdateIcon; Cmd.ofMsg (WritePreferences Player) ]
    | Player.Transition.ExternalMsg.NotifyVolumeChanged -> Cmd.ofMsg (WritePreferences Player)
    | Player.Transition.ExternalMsg.NotifyError text -> Cmd.ofMsg (AddError text)

let init preferences session (startupErrors: string list) =
    let playlistsState, playlistsMsg =
        Playlists.Transition.init session.PlaylistIds preferences.LastTrackId

    { Session = session
      ShowingErrors = isDebug && startupErrors.Length > 0
      Errors = startupErrors |> List.map makeError
      LastNormalSize = preferences.NormalSize
      LastNormalLocation = preferences.NormalLocation
      LastWindowState = preferences.WindowState
      WriteSessionRequests = []
      WritePreferencesRequests = []
      PlaylistsState = playlistsState
      PlayerState = Player.Transition.init preferences.Muted preferences.Volume },
    Cmd.ofMsg (PlaylistsMsg playlistsMsg)

// TODO-NMB: If new Session, call WritePreferences AppSession...
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
          LastSessionId = Some state.Session.Id
          LastTrackId =
            match state.PlaylistsState.PlayerStatus with
            | Some (trackId, playerStatus) when playerStatus <> Errored -> Some trackId
            | _ -> None
          Muted = state.PlayerState.Muted
          Volume = state.PlayerState.Volume }

    let handleResult =
        function
        | Ok _ -> NoOp
        | Error error -> AddError error

    let noChange = state, Cmd.none

    match msg with
    | UpdateTitle ->
        let trackAndPlaylist, cmd =
            match state.PlaylistsState.PlayerStatus with
            | Some (trackId, _) ->
                match Playlists.Transition.findTrack state.PlaylistsState.Playlists trackId with
                | Ok (playlist, trackData) -> $"{trackData.Name} | {playlist.Name} | ", Cmd.none
                | Error error -> "", Cmd.ofMsg (AddError error)
            | None -> "", Cmd.none

        window.Title <- $"{trackAndPlaylist}{state.Session.Name} - {applicationNameAndVersion}"
        state, cmd
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
    | AddError error -> { state with Errors = makeError error :: state.Errors }, Cmd.none
    | ClearError errorId ->
        { state with
            Errors =
                state.Errors
                |> List.filter (fun (otherErrorId, _, _) -> otherErrorId <> errorId) },
        Cmd.none
    | ClearAllErrors -> { state with Errors = [] }, Cmd.none
    | WriteSession ->
        let writeSessionRequestId = WriteSessionRequestId.Create()

        let delay () =
            async {
                do! Async.Sleep DEBOUNCE_WRITE_SESSION_REQUEST_DELAY
                return writeSessionRequestId
            }

        { state with WriteSessionRequests = writeSessionRequestId :: state.WriteSessionRequests },
        Cmd.OfAsync.perform delay () DebounceWriteSessionRequest
    | DebounceWriteSessionRequest writeSessionRequestId ->
        let newWriteSessionRequests =
            state.WriteSessionRequests
            |> List.filter (fun otherWriteSessionRequestId -> otherWriteSessionRequestId <> writeSessionRequestId)

        match newWriteSessionRequests with
        | _ :: _ -> { state with WriteSessionRequests = newWriteSessionRequests }, Cmd.none
        | [] ->
            let newSession =
                { state.Session with
                    PlaylistIds = state.PlaylistsState.Playlists |> List.map (fun playlist -> playlist.Id) }

            { state with
                Session = newSession
                WriteSessionRequests = newWriteSessionRequests },
            Cmd.OfAsync.perform writeSession newSession handleResult
    | WritePreferences source ->
        let writePreferencesRequest = WritePreferencesRequestId.Create(), source

        let delay () =
            async {
                do! Async.Sleep DEBOUNCE_WRITE_PREFERENCES_REQUEST_DELAY
                return writePreferencesRequest
            }

        { state with WritePreferencesRequests = writePreferencesRequest :: state.WritePreferencesRequests },
        Cmd.OfAsync.perform delay () DebounceWritePreferencesRequest
    | DebounceWritePreferencesRequest (writePreferencesRequestId, source) ->
        let newWritePreferencesRequests =
            state.WritePreferencesRequests
            |> List.filter (fun (otherWritePreferencesRequestId, _) ->
                otherWritePreferencesRequestId <> writePreferencesRequestId)

        match
            newWritePreferencesRequests
            |> List.filter (fun (_, otherSource) -> otherSource = source)
        with
        | _ :: _ -> { state with WritePreferencesRequests = newWritePreferencesRequests }, Cmd.none
        | [] ->
            let write =
                match source with
                | AppWindow ->
                    (window.WindowState = WindowState.Normal
                     && ((window.Width, window.Height) <> state.LastNormalSize
                         || (window.Position.X, window.Position.Y) <> state.LastNormalLocation))
                    || (window.WindowState <> WindowState.Minimized
                        && window.WindowState <> state.LastWindowState)
                | AppSession
                | Playlists
                | Player -> true

            if write then
                let preferences = preferences ()

                { state with
                    LastNormalSize = preferences.NormalSize
                    LastNormalLocation = preferences.NormalLocation
                    LastWindowState = preferences.WindowState
                    WritePreferencesRequests = newWritePreferencesRequests },
                Cmd.OfAsync.perform writePreferences preferences handleResult
            else
                { state with WritePreferencesRequests = newWritePreferencesRequests }, Cmd.none
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
    | NoOp -> noChange
    | PlaylistsMsg playlistsMsg ->
        let newPlaylistState, cmd, externalMsgs =
            Playlists.Transition.transition playlistsMsg state.PlaylistsState

        let externalCmds = externalMsgs |> List.map handlePlaylistsExternal

        { state with PlaylistsState = newPlaylistState }, Cmd.batch [ Cmd.map PlaylistsMsg cmd; yield! externalCmds ]
    | PlayerMsg playerMsg ->
        let newPlayerState, cmd, externalMsgs =
            Player.Transition.transition playerMsg state.PlayerState player

        let externalCmds = externalMsgs |> List.map handlePlayerExternal

        { state with PlayerState = newPlayerState }, Cmd.batch [ Cmd.map PlayerMsg cmd; yield! externalCmds ]
    | PlayerPlaying -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyPlaying)
    | PlayerPaused -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyPaused)
    | PlayerStopped -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyStopped)
    | PlayerEnded -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyEnded)
    | PlayerPositionChanged position ->
        state, Cmd.map PlayerMsg (Cmd.ofMsg (Player.Transition.Msg.NotifyPositionChanged position))
    | PlayerErrored -> state, Cmd.map PlayerMsg (Cmd.ofMsg Player.Transition.Msg.NotifyPlaybackErrored)
