module Aornota.Fap.App.Model

open Aornota.Fap
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
      PlaylistIds: Playlists.Model.PlaylistId list
      LastTrackId: Playlists.Model.TrackId option }

type SessionSummary =
    { SessionId: SessionId
      Name: string
      PlaylistCount: int }

type PlaylistSummary =
    { PlaylistId: Playlists.Model.PlaylistId
      Name: string
      TrackCount: int }

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
    | Host
    | App
    | Player

type State =
    { Session: Session
      SessionSummaries: SessionSummary list
      PlaylistSummaries: PlaylistSummary list
      AutoPlaySession: bool
      ShowingErrors: bool
      Errors: (ErrorId * DateTime * string) list
      LastNormalSize: float * float
      LastNormalLocation: int * int
      LastWindowState: WindowState
      WriteSessionRequests: WriteSessionRequestId list
      WritePreferencesRequests: (WritePreferencesRequestId * WritePreferencesRequestSource) list
      PlaylistsState: Playlists.Model.State }

[<Literal>]
let private NEW_SESSION = "new session"

let private sessionFile (SessionId guid) = $"{guid}.{fileExtension Session}"

let newSession () =
    { Id = SessionId.Create()
      Name = NEW_SESSION
      PlaylistIds = []
      LastTrackId = None }

let readSession sessionId =
    async { return! read<Session> Session (sessionFile sessionId) }

let writeSession session =
    async { return! write Session (sessionFile session.Id) session }

let deleteSession sessionId =
    async { return! delete Session (sessionFile sessionId) }

let applicationIcon variant muted =
    let muted = if muted then $"-{ICON_VARIANT_MUTED}" else ""
    WindowIcon(Bitmap.FromImageAsset($"fap-{variant}{muted}.png"))

let applicationNameAndVersion = $"{APPLICATION_NAME} ({APPLICATION_VERSION})"
