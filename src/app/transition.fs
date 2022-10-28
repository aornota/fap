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
    | ClearError of ErrorId
    | ClearAllErrors
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
let private DEBOUNCE_WRITE_PREFERENCES_REQUEST_DELAY = 250

let private makeError error =
    ErrorId.Create(), DateTime.UtcNow, error

let private handlePlaylistsExternal msg =
    match msg with
    | Playlists.Transition.ExternalMsg.RequestTrack (trackData, playlistName, hasPrevious, hasNext, play) ->
        Cmd.batch
            [ Cmd.ofMsg (
                  PlayerMsg(
                      Player.Transition.Msg.NotifyTrackRequested(trackData, playlistName, hasPrevious, hasNext, play)
                  )
              )
              Cmd.ofMsg (UpdateTitle(trackData, playlistName)) ]
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
    | Player.Transition.ExternalMsg.NotifyError text -> Cmd.ofMsg (AddError text)
    | Player.Transition.ExternalMsg.NotifyMutedToggled ->
        Cmd.batch [ Cmd.ofMsg UpdateIcon; Cmd.ofMsg (WritePreferences Player) ]
    | Player.Transition.ExternalMsg.NotifyVolumeChanged -> Cmd.ofMsg (WritePreferences Player)

// TODO-NMB: Remove this once Session persistence implemented...
let private tempPlaylistIds =
    [ Playlists.Model.PlaylistId(Guid("e7cc6cd6-7ef3-404c-bdc8-02c285c064fe")) // wip (mellow)
      Playlists.Model.PlaylistId(Guid("67367509-813f-4266-b217-e41e9c3f53f5")) // now we are 03
      Playlists.Model.PlaylistId(Guid("3aff9190-f542-43d2-a50d-a032b4d840a9")) // sss0018 (for nick & olivia)
      Playlists.Model.PlaylistId(Guid("4250968a-d4cc-4fa5-bbcc-f2f5c9f3d3c9")) ] // new playlist

let init preferences preferencesErrors =
    let errors = preferencesErrors |> List.map makeError

    // TODO-NMB: Get from "Session"...
    let playlistIds = tempPlaylistIds

    let playlistsState, playlistsMsg = Playlists.Transition.init playlistIds

    let playerState = Player.Transition.init preferences.Muted preferences.Volume

    { ShowingErrors = isDebug && errors.Length > 0
      Errors = errors
      LastNormalSize = preferences.NormalSize
      LastNormalLocation = preferences.NormalLocation
      LastWindowState = preferences.WindowState
      WritePreferencesRequests = []
      PlaylistsState = playlistsState
      PlayerState = playerState },
    Cmd.ofMsg (PlaylistsMsg playlistsMsg)

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
    | AddError error -> { state with Errors = makeError error :: state.Errors }, Cmd.none
    | ClearError errorId ->
        { state with
            Errors =
                state.Errors
                |> List.filter (fun (otherErrorId, _, _) -> otherErrorId <> errorId) },
        Cmd.none
    | ClearAllErrors -> { state with Errors = [] }, Cmd.none
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
                | App ->
                    (window.WindowState = WindowState.Normal
                     && ((window.Width, window.Height) <> state.LastNormalSize
                         || (window.Position.X, window.Position.Y) <> state.LastNormalLocation))
                    || (window.WindowState <> WindowState.Minimized
                        && window.WindowState <> state.LastWindowState)
                | Player -> true

            if write then
                let preferences = preferences ()

                { state with
                    LastNormalSize = preferences.NormalSize
                    LastNormalLocation = preferences.NormalLocation
                    LastWindowState = preferences.WindowState
                    WritePreferencesRequests = newWritePreferencesRequests },
                Cmd.OfAsync.perform writePreferences preferences handleWritePreferencesResult
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
