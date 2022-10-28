module Aornota.Fap.App.Model

open Aornota.Fap
open Aornota.Fap.Domain
open Aornota.Fap.Literals
open Aornota.Fap.Persistence
open Avalonia.Controls
open Avalonia.Media.Imaging
open System

type SessionId =
    | SessionId of Guid

    static member Create() = SessionId(Guid.NewGuid())

type Session =
    { Id: SessionId
      Name: string
      PlaylistIds: Playlists.Model.PlaylistId list }

type Preferences = // TODO-NMB: LastAudioFolder?...
    { NormalSize: float * float
      NormalLocation: int * int
      WindowState: WindowState
      LastSessionId: SessionId option
      LastTrackId: TrackId option
      Muted: bool
      Volume: int }

type ErrorId =
    | ErrorId of Guid

    static member Create() = ErrorId(Guid.NewGuid())

type WriteSessionRequestId =
    | WriteSessionRequestId of Guid

    static member Create() = WriteSessionRequestId(Guid.NewGuid())

type WritePreferencesRequestId =
    | WritePreferencesRequestId of Guid

    static member Create() =
        WritePreferencesRequestId(Guid.NewGuid())

type WritePreferencesRequestSource =
    | AppWindow
    | AppSession
    | Playlists
    | Player

type State =
    { Session: Session
      ShowingErrors: bool
      Errors: (ErrorId * DateTime * string) list
      LastNormalSize: float * float
      LastNormalLocation: int * int
      LastWindowState: WindowState
      WriteSessionRequests: WriteSessionRequestId list
      WritePreferencesRequests: (WritePreferencesRequestId * WritePreferencesRequestSource) list
      PlaylistsState: Playlists.Model.State
      PlayerState: Player.Model.State }

[<Literal>]
let MIN_WIDTH = 800.

[<Literal>]
let MIN_HEIGHT = 600.

[<Literal>]
let NEW_SESSION = "new session"

let private preferencesFile =
    $"{Environment.UserName.ToLowerInvariant()}.{fileExtension Preferences}"

let private sessionFile (SessionId guid) = $"{guid}.{fileExtension Session}"

let readSession sessionId =
    async { return! read<Session> Session (sessionFile sessionId) }

let writeSession session =
    async { return! write Session (sessionFile session.Id) session }

let readPreferences () =
    async { return! read<Preferences> Preferences preferencesFile }

let writePreferences (preferences: Preferences) =
    async { return! write Preferences preferencesFile preferences }

let applicationNameAndVersion = $"{APPLICATION_NAME} ({APPLICATION_VERSION})"

let applicationIcon playerStatus muted =
    let variant =
        match playerStatus with
        | Active -> "active"
        | Awaiting -> "awaiting"
        | Inactive -> "inactive"
        | Errored -> "errored"

    let muted = if muted then "-muted" else ""
    WindowIcon(Bitmap.FromImageAsset($"fap-{variant}{muted}.png"))
