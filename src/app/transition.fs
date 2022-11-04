module Aornota.Fap.App.Transition

open Aornota.Fap
open Aornota.Fap.App.Model
open Aornota.Fap.App.Preferences
open Aornota.Fap.Persistence
open Aornota.Fap.Utilities
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Hosts
open Elmish
open LibVLCSharp.Shared
open System

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
    | PlaylistDeleted of Playlists.Model.PlaylistId
    | AddError of string
    | UpdateSessionSummary
    | WriteSession
    | DebounceWriteSessionRequest of WriteSessionRequestId
    | WritePreferences of WritePreferencesRequestSource
    | DebounceWritePreferencesRequest of WritePreferencesRequestId * WritePreferencesRequestSource
    // UI
    | OnNewSession
    | OnOpenSession of SessionId
    | OnDeleteSession of SessionId
    | OnToggleAutoPlaySession
    | OnExit
    | OnNewPlaylist
    | OnOpenPlaylist of Playlists.Model.PlaylistId
    // TODO-NMB...OnAddFiles
    // TODO-NMB...OnAddFolder
    | OnDeletePlaylist of Playlists.Model.PlaylistId
    | OnToggleShowingErrors
    | OnClearAllErrors
    | OnClearError of ErrorId
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

[<Literal>]
let private DEBOUNCE_WRITE_SESSION_REQUEST_DELAY = 250

[<Literal>]
let private DEBOUNCE_WRITE_PREFERENCES_REQUEST_DELAY = 250

let private shutdown () =
    match Avalonia.Application.Current.ApplicationLifetime with
    | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.Shutdown 0
    | _ -> ()

let private makeError error =
    ErrorId.Create(), DateTime.UtcNow, error

let private sessionSummary (session: Session) =
    { SessionId = session.Id
      Name = session.Name
      PlaylistCount = session.PlaylistIds.Length }

let private playlistSummary (playlist: Playlists.Model.Playlist) =
    { PlaylistId = playlist.Id
      Name = playlist.Name
      TrackCount = Playlists.Model.tracks playlist |> List.length }

let private handleResult =
    function
    | Ok _ -> NoOp
    | Error error -> AddError error

let private handlePlaylistsExternal externalMsg =
    match externalMsg with
    | Playlists.Transition.ExternalMsg.NotifyNewPlaylistAdded playlist -> Cmd.ofMsg (NewPlaylistAdded playlist)
    | Playlists.Transition.ExternalMsg.NotifyPlaylistsChanged -> Cmd.ofMsg UpdateSessionSummary
    | Playlists.Transition.ExternalMsg.NotifyTrackStateChanged ->
        Cmd.batch [ Cmd.ofMsg UpdateTitle; Cmd.ofMsg UpdateIcon; Cmd.ofMsg WriteSession ]
    | Playlists.Transition.ExternalMsg.NotifyMutedToggled ->
        Cmd.batch [ Cmd.ofMsg UpdateIcon; Cmd.ofMsg (WritePreferences Player) ]
    | Playlists.Transition.ExternalMsg.NotifyVolumeChanged -> Cmd.ofMsg (WritePreferences Player)
    | Playlists.Transition.ExternalMsg.NotifyError error -> Cmd.ofMsg (AddError error)

let init preferences session sessionIds playlistIds (startupErrors: string list) (player: MediaPlayer) =
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
      ShowingErrors = isDebug && startupErrors.Length > 0
      Errors = startupErrors |> List.map makeError
      LastNormalSize = preferences.NormalSize
      LastNormalLocation = preferences.NormalLocation
      LastWindowState = preferences.WindowState
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
    | ReadSessionError (readError, SessionId guid, sessionIds, summaries) ->
        state,
        Cmd.batch
            [ Cmd.ofMsg (ReadSessions(sessionIds, summaries))
              Cmd.ofMsg (
                  AddError $"App.transition -> Unable to read {nameof (Session)} ({guid}): {readErrorText readError}"
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
    | ReadPlaylistError (readError, Playlists.Model.PlaylistId guid, playlistIds, summaries) ->
        state,
        Cmd.batch
            [ Cmd.ofMsg (ReadPlaylists(playlistIds, summaries))
              Cmd.ofMsg (
                  AddError $"App.transition -> Unable to read {nameof (Playlist)} ({guid}): {readErrorText readError}"
              ) ]
    | UpdateTitle ->
        let trackAndPlaylist, cmd =
            match state.PlaylistsState.TrackState with
            | Some trackState ->
                match Playlists.Model.findTrack state.PlaylistsState.Playlists trackState.Track.Id with
                | Ok (playlist, trackData) -> $"{trackData.Name} | {playlist.Name} | ", Cmd.none
                | Error error -> "", Cmd.ofMsg (AddError error)
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
        | None -> state, Cmd.ofMsg (AddError $"App.transition -> {nameof (Session)} ({sessionId} not in summaries")
    | NewPlaylistAdded playlist ->
        match
            state.PlaylistSummaries
            |> List.tryFind (fun summary -> summary.PlaylistId = playlist.Id)
        with
        | Some _ ->
            state,
            Cmd.ofMsg (
                AddError $"App.transition -> {nameof (Playlists.Model.Playlist)} ({playlist.Id} already in summaries"
            )
        | None ->
            { state with PlaylistSummaries = playlistSummary playlist :: state.PlaylistSummaries },
            Cmd.ofMsg UpdateSessionSummary
    | PlaylistOpened playlist ->
        state, Cmd.map PlaylistsMsg (Cmd.ofMsg (Playlists.Transition.Msg.NotifyPlaylistOpened playlist))
    | PlaylistDeleted playilstId ->
        match
            state.PlaylistSummaries
            |> List.tryFind (fun summary -> summary.PlaylistId = playilstId)
        with
        | Some _ ->
            let newPlaylistSummaries =
                state.PlaylistSummaries
                |> List.filter (fun summary -> summary.PlaylistId <> playilstId)

            { state with PlaylistSummaries = newPlaylistSummaries }, Cmd.none
        | None ->
            state,
            Cmd.ofMsg (AddError $"App.transition -> {nameof (Playlists.Model.Playlist)} ({playilstId} not in summaries")
    | AddError error -> { state with Errors = makeError error :: state.Errors }, Cmd.none
    | UpdateSessionSummary ->
        let newSessionSummary =
            { SessionId = state.Session.Id
              Name = state.Session.Name
              PlaylistCount = state.PlaylistsState.Playlists.Length }

        match
            state.SessionSummaries
            |> List.tryFind (fun summary -> summary.SessionId = newSessionSummary.SessionId)
        with
        | Some _ ->
            let newSessionSummaries =
                state.SessionSummaries
                |> List.map (fun summary ->
                    if summary.SessionId = newSessionSummary.SessionId then
                        newSessionSummary
                    else
                        summary)

            { state with SessionSummaries = newSessionSummaries }, Cmd.ofMsg WriteSession
        | None ->
            state,
            Cmd.ofMsg (AddError $"App.transition -> {nameof (Session)} ({newSessionSummary.SessionId} not in summaries")
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

                { state with
                    Session = newSession
                    WriteSessionRequests = newWriteSessionRequests },
                Cmd.OfAsync.perform writeSession newSession handleResult
            else
                { state with WriteSessionRequests = newWriteSessionRequests }, Cmd.none
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
                      Muted = state.PlaylistsState.Muted
                      Volume = state.PlaylistsState.Volume }

                { state with
                    LastNormalSize = preferences.NormalSize
                    LastNormalLocation = preferences.NormalLocation
                    LastWindowState = preferences.WindowState
                    WritePreferencesRequests = newWritePreferencesRequests },
                Cmd.OfAsync.perform writePreferences preferences handleResult
            else
                { state with WritePreferencesRequests = newWritePreferencesRequests }, Cmd.none
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
                AddError $"App.transition -> Unable to read {nameof (Session)} ({guid}): {readErrorText readError}"

        state, Cmd.OfAsync.perform read () handleResult
    | OnDeleteSession sessionId ->
        let delete () =
            async { return! deleteSession sessionId }

        let handleResult =
            function
            | Ok () -> SessionDeleted sessionId
            | Error error ->
                let (SessionId guid) = sessionId
                AddError $"App.transition -> Unable to delete {nameof (Session)} ({guid}): {error}"

        state, Cmd.OfAsync.perform delete () handleResult
    | OnToggleAutoPlaySession ->
        { state with AutoPlaySession = not state.AutoPlaySession }, Cmd.ofMsg (WritePreferences App)
    | OnExit ->
        shutdown ()
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

                AddError
                    $"App.transition -> Unable to read {nameof (Playlists.Model.Playlist)} ({guid}): {readErrorText readError}"

        state, Cmd.OfAsync.perform read () handleResult
    // TODO-NMB: OnAddFiles...
    // TODO-NMB: OnAddFolder...
    | OnDeletePlaylist playlistId ->
        let delete () =
            async { return! Playlists.Model.deletePlaylist playlistId }

        let handleResult =
            function
            | Ok () -> PlaylistDeleted playlistId
            | Error error ->
                let (Playlists.Model.PlaylistId guid) = playlistId
                AddError $"App.transition -> Unable to delete {nameof (Playlists.Model.Playlist)} ({guid}): {error}"

        state, Cmd.OfAsync.perform delete () handleResult
    | OnToggleShowingErrors -> { state with ShowingErrors = not state.ShowingErrors }, Cmd.none
    | OnClearAllErrors -> { state with Errors = [] }, Cmd.none
    | OnClearError errorId ->
        { state with
            Errors =
                state.Errors
                |> List.filter (fun (otherErrorId, _, _) -> otherErrorId <> errorId) },
        Cmd.none
    // From Playlists
    | PlaylistsMsg playlistsMsg ->
        let newPlaylistState, cmd, externalMsgs =
            Playlists.Transition.transition playlistsMsg state.PlaylistsState player

        { state with PlaylistsState = newPlaylistState },
        Cmd.batch
            [ Cmd.map PlaylistsMsg cmd
              yield! externalMsgs |> List.map handlePlaylistsExternal ]
    // From HostWindow subscriptions
    | LocationChanged -> state, Cmd.ofMsg (WritePreferences Host)
    | EffectiveViewportChanged -> state, Cmd.ofMsg (WritePreferences Host)
    // From MediaPlayer subscriptions
    | PlayerErrored -> state, Cmd.map PlaylistsMsg (Cmd.ofMsg Playlists.Transition.Msg.NotifyErrored)
    | PlayerPlaying -> state, Cmd.map PlaylistsMsg (Cmd.ofMsg Playlists.Transition.Msg.NotifyPlaying)
    | PlayerPositionChanged position ->
        state, Cmd.map PlaylistsMsg (Cmd.ofMsg (Playlists.Transition.Msg.NotifyPositionChanged position))
    | PlayerEnded -> state, Cmd.map PlaylistsMsg (Cmd.ofMsg Playlists.Transition.Msg.NotifyEnded)
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
