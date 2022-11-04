module Aornota.Fap.App.Transition

open Aornota.Fap
open Aornota.Fap.App.Model
open Aornota.Fap.App.Preferences
open Aornota.Fap.Literals
open Aornota.Fap.Persistence
open Aornota.Fap.Utilities
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Hosts
open Elmish
open LibVLCSharp.Shared
open System
open System.Collections.Generic
open System.IO

type Msg =
    // Internal
    | NoOp
    | ReadSessions of SessionId list * SessionSummary list
    | ReadSessionError of ReadError * SessionId * SessionId list * SessionSummary list
    | ReadPlaylists of Playlists.Model.PlaylistId list * PlaylistSummary list
    | ReadPlaylistError of
        ReadError *
        Playlists.Model.PlaylistId *
        Playlists.Model.PlaylistId list *
        PlaylistSummary list
    | UpdateTitle
    | UpdateIcon
    | SessionOpened of Session
    | SessionDeleted of SessionId
    | NewPlaylistAdded of Playlists.Model.Playlist
    | PlaylistOpened of Playlists.Model.Playlist
    | FilesAdded of string array
    | FolderAdded of string
    | PlaylistDeleted of Playlists.Model.PlaylistId
    | UpdateSessionSummary
    | UpdatePlaylistSummary of Playlists.Model.Playlist
    | WriteSession
    | DebounceWriteSessionRequest of WriteSessionRequestId
    | WritePreferences of WritePreferencesRequestSource
    | DebounceWritePreferencesRequest of WritePreferencesRequestId * WritePreferencesRequestSource
    | AddError of string * string option
    // UI
    | OnNewSession
    | OnOpenSession of SessionId
    | OnDeleteSession of SessionId
    | OnToggleAutoPlaySession
    | OnExit
    | OnNewPlaylist
    | OnOpenPlaylist of Playlists.Model.PlaylistId
    | OnAddFiles
    | OnAddFolder
    | OnDeletePlaylist of Playlists.Model.PlaylistId
    | OnToggleShowingDebugOnlyErrors
    | OnClearAllErrors
    | OnDismissError of ErrorId
    // From Playlists
    | PlaylistsMsg of Playlists.Transition.Msg
    // From HostWindow subscriptions
    | LocationChanged
    | EffectiveViewportChanged
    // From MediaPlayer subscriptions
    | PlayerErrored
    | PlayerPlaying
    | PlayerPositionChanged of float32
    | PlayerEnded

let private sessionSummary (session: Session) =
    { SessionId = session.Id
      Name = session.Name
      PlaylistCount = session.PlaylistIds.Length }

let private playlistSummary (playlist: Playlists.Model.Playlist) =
    { PlaylistId = playlist.Id
      Name = playlist.Name
      TrackCount = Playlists.Model.tracks playlist |> List.length }

let private errorOrNoOp nonDebugMessage =
    function
    | Ok _ -> NoOp
    | Error error -> AddError(error, nonDebugMessage)

let init
    preferences
    session
    sessionIds
    playlistIds
    (startupErrors: (ErrorId * DateTime * string * string option) list)
    (player: MediaPlayer)
    =
    let playlistsState, playlistsMsgs =
        Playlists.Transition.init
            session.PlaylistIds
            session.LastTrackId
            preferences.Muted
            preferences.Volume
            preferences.AutoPlaySession
            player

    let sessionSummaries = [ sessionSummary session ]

    let sessionIds =
        sessionIds |> List.filter (fun sessionId -> sessionId <> session.Id)

    { Session = session
      SessionSummaries = sessionSummaries
      PlaylistSummaries = []
      AutoPlaySession = preferences.AutoPlaySession
      ShowingDebugOnlyErrors = isDebug && startupErrors.Length > 0
      Errors = startupErrors
      LastNormalSize = preferences.NormalSize
      LastNormalLocation = preferences.NormalLocation
      LastWindowState = preferences.WindowState
      LastAudioFolder = preferences.LastAudioFolder
      WriteSessionRequests = []
      WritePreferencesRequests = []
      PlaylistsState = playlistsState },
    Cmd.batch
        [ Cmd.ofMsg (ReadSessions(sessionIds, sessionSummaries))
          Cmd.ofMsg (ReadPlaylists(playlistIds, []))
          yield! playlistsMsgs |> List.map (Cmd.ofMsg >> (Cmd.map PlaylistsMsg)) ]

let transition msg (state: State) (window: HostWindow) (player: MediaPlayer) =
    let noChange = state, Cmd.none

    match msg with
    // Internal
    | NoOp -> noChange
    | ReadSessions (sessionIds, summaries) ->
        match sessionIds with
        | sessionId :: sessionIds ->
            let read () = async { return! readSession sessionId }

            let handleResult =
                function
                | Ok session -> ReadSessions(sessionIds, sessionSummary session :: summaries)
                | Error error -> ReadSessionError(error, sessionId, sessionIds, summaries)

            state, Cmd.OfAsync.perform read () handleResult
        | [] -> { state with SessionSummaries = summaries }, Cmd.none
    | ReadSessionError (readError, sessionId, sessionIds, summaries) ->
        let (SessionId guid) = sessionId

        state,
        Cmd.batch
            [ Cmd.ofMsg (ReadSessions(sessionIds, summaries))
              Cmd.ofMsg (
                  AddError(
                      $"{nameof (ReadSessionError)} ({sessionId}): {readErrorText readError}",
                      Some $"Unable to read session ({guid})"
                  )
              ) ]
    | ReadPlaylists (playlistIds, summaries) ->
        match playlistIds with
        | playlistId :: playlistIds ->
            let read () =
                async { return! Playlists.Model.readPlaylist playlistId }

            let handleResult =
                function
                | Ok playlist -> ReadPlaylists(playlistIds, playlistSummary playlist :: summaries)
                | Error error -> ReadPlaylistError(error, playlistId, playlistIds, summaries)

            state, Cmd.OfAsync.perform read () handleResult
        | [] -> { state with PlaylistSummaries = summaries }, Cmd.none
    | ReadPlaylistError (readError, playlistId, playlistIds, summaries) ->
        let (Playlists.Model.PlaylistId guid) = playlistId

        state,
        Cmd.batch
            [ Cmd.ofMsg (ReadPlaylists(playlistIds, summaries))
              Cmd.ofMsg (
                  AddError(
                      $"{nameof (ReadPlaylistError)} ({playlistId}): {readErrorText readError}",
                      Some $"Unable to read playilst ({guid})"
                  )
              ) ]
    | UpdateTitle ->
        let trackAndPlaylist, cmd =
            match state.PlaylistsState.TrackState with
            | Some trackState ->
                match Playlists.Model.findTrack state.PlaylistsState.Playlists trackState.Track.Id with
                | Ok (playlist, trackData) -> $"{trackData.Name} | {playlist.Name} | ", Cmd.none
                | Error error -> "", Cmd.ofMsg (AddError(error, None))
            | None -> "", Cmd.none

        window.Title <- $"{trackAndPlaylist}{state.Session.Name} - {applicationNameAndVersion}"
        state, cmd
    | UpdateIcon ->
        let variant = Playlists.Model.iconVariant state.PlaylistsState.TrackState

        window.Icon <- applicationIcon variant state.PlaylistsState.Muted
        noChange
    | SessionOpened session ->
        let newPlaylistsState, playlistsMsgs =
            Playlists.Transition.init
                session.PlaylistIds
                session.LastTrackId
                state.PlaylistsState.Muted
                state.PlaylistsState.Volume
                state.AutoPlaySession
                player

        { state with
            Session = session
            WriteSessionRequests = []
            WritePreferencesRequests = []
            PlaylistsState = newPlaylistsState },
        Cmd.batch
            [ Cmd.ofMsg (WritePreferences App)
              yield! playlistsMsgs |> List.map (Cmd.ofMsg >> (Cmd.map PlaylistsMsg)) ]
    | SessionDeleted sessionId ->
        match
            state.SessionSummaries
            |> List.tryFind (fun summary -> summary.SessionId = sessionId)
        with
        | Some _ ->
            let newSessionSummaries =
                state.SessionSummaries
                |> List.filter (fun summary -> summary.SessionId <> sessionId)

            { state with SessionSummaries = newSessionSummaries }, Cmd.none
        | None -> state, Cmd.ofMsg (AddError($"{nameof (SessionDeleted)}: ({sessionId}) not in summaries", None))
    | NewPlaylistAdded playlist ->
        match
            state.PlaylistSummaries
            |> List.tryFind (fun summary -> summary.PlaylistId = playlist.Id)
        with
        | Some _ ->
            state, Cmd.ofMsg (AddError($"{nameof (NewPlaylistAdded)}: ({playlist.Id}) already in summaries", None))
        | None ->
            { state with PlaylistSummaries = playlistSummary playlist :: state.PlaylistSummaries },
            Cmd.ofMsg UpdateSessionSummary
    | PlaylistOpened playlist ->
        state, Cmd.map PlaylistsMsg (Cmd.ofMsg (Playlists.Transition.Msg.NotifyPlaylistOpened playlist))
    | FilesAdded files ->
        match files with
        | null -> noChange
        | _ ->
            let files = files |> List.ofArray

            match files with
            | file :: _ ->
                let lastAudioFolder = FileInfo(file).Directory.FullName

                { state with LastAudioFolder = lastAudioFolder },
                Cmd.batch
                    [ Cmd.ofMsg (WritePreferences App)
                      Cmd.map PlaylistsMsg (Cmd.ofMsg (Playlists.Transition.Msg.NotifyFilesAdded files)) ]
            | [] -> noChange
    | FolderAdded folder ->
        match folder with
        | null -> noChange
        | _ ->
            let lastAudioFolder =
                match DirectoryInfo(folder).Parent with
                | null -> folder
                | parent -> parent.FullName

            { state with LastAudioFolder = lastAudioFolder },
            Cmd.batch
                [ Cmd.ofMsg (WritePreferences App)
                  Cmd.map PlaylistsMsg (Cmd.ofMsg (Playlists.Transition.Msg.NotifyFolderAdded folder)) ]
    | PlaylistDeleted playlistId ->
        match
            state.PlaylistSummaries
            |> List.tryFind (fun summary -> summary.PlaylistId = playlistId)
        with
        | Some _ ->
            let newPlaylistSummaries =
                state.PlaylistSummaries
                |> List.filter (fun summary -> summary.PlaylistId <> playlistId)

            { state with PlaylistSummaries = newPlaylistSummaries }, Cmd.none
        | None -> state, Cmd.ofMsg (AddError($"{nameof (PlaylistDeleted)}: ({playlistId}) not in summaries", None))
    | UpdateSessionSummary ->
        match
            state.SessionSummaries
            |> List.tryFind (fun summary -> summary.SessionId = state.Session.Id)
        with
        | Some summary ->
            let newSessionSummaries =
                state.SessionSummaries
                |> List.map (fun otherSummary ->
                    if otherSummary.SessionId = summary.SessionId then
                        { summary with PlaylistCount = state.PlaylistsState.Playlists.Length }
                    else
                        otherSummary)

            { state with SessionSummaries = newSessionSummaries }, Cmd.ofMsg WriteSession
        | None ->
            state, Cmd.ofMsg (AddError($"{nameof (UpdateSessionSummary)}: ({state.Session.Id}) not in summaries", None))
    | UpdatePlaylistSummary playlist ->
        match
            state.PlaylistSummaries
            |> List.tryFind (fun summary -> summary.PlaylistId = playlist.Id)
        with
        | Some summary ->
            let newPlaylistSummaries =
                state.PlaylistSummaries
                |> List.map (fun otherSummary ->
                    if otherSummary.PlaylistId = summary.PlaylistId then
                        { summary with TrackCount = Playlists.Model.tracks playlist |> List.length }
                    else
                        otherSummary)

            { state with PlaylistSummaries = newPlaylistSummaries }, Cmd.none
        | None -> state, Cmd.ofMsg (AddError($"{nameof (NewPlaylistAdded)}: ({playlist.Id}) not in summaries", None))
    | WriteSession ->
        let writeSessionRequestId = WriteSessionRequestId.Create()

        let delay () =
            async {
                do! Async.Sleep 500
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
            let newPlaylistIds =
                state.PlaylistsState.Playlists |> List.map (fun playlist -> playlist.Id)

            let newLastTrackId =
                match state.PlaylistsState.TrackState with
                | Some trackState ->
                    match trackState.PlayerState with
                    | Playlists.Model.PlayerState.PlaybackErrored -> None
                    | _ -> Some trackState.Track.Id
                | None -> None

            if
                newPlaylistIds <> state.Session.PlaylistIds
                || newLastTrackId <> state.Session.LastTrackId
            then
                let newSession =
                    { state.Session with
                        PlaylistIds = newPlaylistIds
                        LastTrackId = newLastTrackId }

                let (SessionId guid) = state.Session.Id

                { state with
                    Session = newSession
                    WriteSessionRequests = newWriteSessionRequests },
                Cmd.OfAsync.perform writeSession newSession (errorOrNoOp (Some $"Unable to write sessions ({guid})"))
            else
                { state with WriteSessionRequests = newWriteSessionRequests }, Cmd.none
    | WritePreferences source ->
        let writePreferencesRequest = WritePreferencesRequestId.Create(), source

        let delay () =
            async {
                do! Async.Sleep 500
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
                | Host ->
                    (window.WindowState = WindowState.Normal
                     && ((window.Width, window.Height) <> state.LastNormalSize
                         || (window.Position.X, window.Position.Y) <> state.LastNormalLocation))
                    || (window.WindowState <> WindowState.Minimized
                        && window.WindowState <> state.LastWindowState)
                | App
                | Player -> true

            if write then
                let preferences =
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
                      AutoPlaySession = state.AutoPlaySession
                      LastAudioFolder = state.LastAudioFolder
                      Muted = state.PlaylistsState.Muted
                      Volume = state.PlaylistsState.Volume }

                { state with
                    LastNormalSize = preferences.NormalSize
                    LastNormalLocation = preferences.NormalLocation
                    LastWindowState = preferences.WindowState
                    WritePreferencesRequests = newWritePreferencesRequests },
                Cmd.OfAsync.perform writePreferences preferences (errorOrNoOp (Some "Unable to write preferences"))
            else
                { state with WritePreferencesRequests = newWritePreferencesRequests }, Cmd.none
    | AddError (error, nonDebugMessage) ->
        let maxErrors = 100

        let newError = makeError error nonDebugMessage

        let newErrors =
            newError
            :: (if state.Errors.Length > (maxErrors - 1) then
                    state.Errors |> List.take (maxErrors - 1)
                else
                    state.Errors)

        { state with Errors = newErrors }, Cmd.none
    // UI
    | OnNewSession ->
        let session = newSession ()

        let newPlaylistsState, playlistsMsgs =
            Playlists.Transition.init
                session.PlaylistIds
                session.LastTrackId
                state.PlaylistsState.Muted
                state.PlaylistsState.Volume
                state.AutoPlaySession
                player

        { state with
            Session = session
            SessionSummaries = sessionSummary session :: state.SessionSummaries
            WriteSessionRequests = []
            WritePreferencesRequests = []
            PlaylistsState = newPlaylistsState },
        // Note: No need to call WriteSession as should be called once Playlists sends NotifyTrackStateChanged (and calling explicitly could cause errors due to concurrent writes),
        Cmd.batch
            [ Cmd.ofMsg (WritePreferences App)
              yield! playlistsMsgs |> List.map (Cmd.ofMsg >> (Cmd.map PlaylistsMsg)) ]
    | OnOpenSession sessionId ->
        let read () = async { return! readSession sessionId }

        let handleResult =
            function
            | Ok session -> SessionOpened session
            | Error readError ->
                let (SessionId guid) = sessionId

                AddError(
                    $"{nameof (OnOpenSession)} ({sessionId}): {readErrorText readError}",
                    Some $"Unable to open session ({guid})"
                )

        state, Cmd.OfAsync.perform read () handleResult
    | OnDeleteSession sessionId ->
        let delete () =
            async { return! deleteSession sessionId }

        let handleResult =
            function
            | Ok () -> SessionDeleted sessionId
            | Error error ->
                let (SessionId guid) = sessionId

                AddError(
                    $"{nameof (OnDeleteSession)} ({sessionId}): {error}",
                    Some $"Unable to delete session ({guid})"
                )

        state, Cmd.OfAsync.perform delete () handleResult
    | OnToggleAutoPlaySession ->
        { state with AutoPlaySession = not state.AutoPlaySession }, Cmd.ofMsg (WritePreferences App)
    | OnExit ->
        match Avalonia.Application.Current.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.Shutdown 0
        | _ -> ()

        noChange
    | OnNewPlaylist -> state, Cmd.map PlaylistsMsg (Cmd.ofMsg Playlists.Transition.Msg.NotifyNewPlaylistRequested)
    | OnOpenPlaylist playlistId ->
        let read () =
            async { return! Playlists.Model.readPlaylist playlistId }

        let handleResult =
            function
            | Ok playlist -> PlaylistOpened playlist
            | Error readError ->
                let (Playlists.Model.PlaylistId guid) = playlistId

                AddError(
                    $"{nameof (OnOpenPlaylist)} ({playlistId}): {readErrorText readError}",
                    Some $"Unable to open playlist ({guid})"
                )

        state, Cmd.OfAsync.perform read () handleResult
    | OnAddFiles ->
        let filter = FileDialogFilter()

        filter.Extensions <- List(fileExtensions)
        filter.Name <- "Audio files"

        let dialog = OpenFileDialog()

        dialog.Directory <- state.LastAudioFolder
        dialog.Title <- ADD_FILES
        dialog.AllowMultiple <- true
        dialog.Filters <- List(seq { filter })

        let showDialog () =
            dialog.ShowAsync(window) |> Async.AwaitTask

        state, Cmd.OfAsync.perform showDialog () FilesAdded
    | OnAddFolder ->
        let dialog = OpenFolderDialog()
        dialog.Directory <- state.LastAudioFolder
        dialog.Title <- ADD_FOLDER

        let showDialog () =
            dialog.ShowAsync(window) |> Async.AwaitTask

        state, Cmd.OfAsync.perform showDialog () FolderAdded
    | OnDeletePlaylist playlistId ->
        let delete () =
            async { return! Playlists.Model.deletePlaylist playlistId }

        let handleResult =
            function
            | Ok () -> PlaylistDeleted playlistId
            | Error error ->
                let (Playlists.Model.PlaylistId guid) = playlistId

                AddError(
                    $"{nameof (OnDeletePlaylist)} ({playlistId}): {error}",
                    Some $"Unable to delete playlist ({guid})"
                )

        state, Cmd.OfAsync.perform delete () handleResult
    | OnToggleShowingDebugOnlyErrors ->
        { state with ShowingDebugOnlyErrors = not state.ShowingDebugOnlyErrors }, Cmd.none
    | OnClearAllErrors -> { state with Errors = [] }, Cmd.none
    | OnDismissError errorId ->
        { state with
            Errors =
                state.Errors
                |> List.filter (fun (otherErrorId, _, _, _) -> otherErrorId <> errorId) },
        Cmd.none
    // From Playlists
    | PlaylistsMsg playlistsMsg ->
        let handleExternal externalMsg =
            match externalMsg with
            | Playlists.Transition.ExternalMsg.NotifyNewPlaylistAdded playlist -> Cmd.ofMsg (NewPlaylistAdded playlist)
            | Playlists.Transition.ExternalMsg.NotifyTracksAddedOrRemoved playlist ->
                Cmd.ofMsg (UpdatePlaylistSummary playlist)
            | Playlists.Transition.ExternalMsg.NotifyPlaylistsChanged -> Cmd.ofMsg UpdateSessionSummary
            | Playlists.Transition.ExternalMsg.NotifyTrackStateChanged ->
                Cmd.batch [ Cmd.ofMsg UpdateTitle; Cmd.ofMsg UpdateIcon; Cmd.ofMsg WriteSession ]
            | Playlists.Transition.ExternalMsg.NotifyMutedToggled ->
                Cmd.batch [ Cmd.ofMsg UpdateIcon; Cmd.ofMsg (WritePreferences Player) ]
            | Playlists.Transition.ExternalMsg.NotifyVolumeChanged -> Cmd.ofMsg (WritePreferences Player)
            | Playlists.Transition.ExternalMsg.NotifyError (error, nonDebugMessage) ->
                Cmd.ofMsg (AddError(error, nonDebugMessage))

        let newPlaylistState, cmd, externalMsgs =
            Playlists.Transition.transition playlistsMsg state.PlaylistsState player

        { state with PlaylistsState = newPlaylistState },
        Cmd.batch [ Cmd.map PlaylistsMsg cmd; yield! externalMsgs |> List.map handleExternal ]
    // From HostWindow subscriptions
    | LocationChanged -> state, Cmd.ofMsg (WritePreferences Host)
    | EffectiveViewportChanged -> state, Cmd.ofMsg (WritePreferences Host)
    // From MediaPlayer subscriptions
    | PlayerErrored -> state, Cmd.map PlaylistsMsg (Cmd.ofMsg Playlists.Transition.Msg.NotifyErrored)
    | PlayerPlaying -> state, Cmd.map PlaylistsMsg (Cmd.ofMsg Playlists.Transition.Msg.NotifyPlaying)
    | PlayerPositionChanged position ->
        state, Cmd.map PlaylistsMsg (Cmd.ofMsg (Playlists.Transition.Msg.NotifyPositionChanged position))
    | PlayerEnded -> state, Cmd.map PlaylistsMsg (Cmd.ofMsg Playlists.Transition.Msg.NotifyEnded)
