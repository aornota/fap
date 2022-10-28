module Aornota.Fap.App.Model

open Aornota.Fap
open Aornota.Fap.Domain
open Aornota.Fap.Literals
open Aornota.Fap.Persistence
open Avalonia.Controls
open Avalonia.Media.Imaging
open System

// TODO-NMB: LastFolder? "Session"?...
type Preferences =
    { NormalSize: float * float
      NormalLocation: int * int
      WindowState: WindowState
      Muted: bool
      Volume: int }

type ErrorId =
    | ErrorId of Guid

    static member Create() = ErrorId(Guid.NewGuid())

type WritePreferencesRequestId =
    | WritePreferencesRequestId of Guid

    static member Create() =
        WritePreferencesRequestId(Guid.NewGuid())

type WritePreferencesRequestSource =
    | App
    | Player

type State =
    { ShowingErrors: bool
      Errors: (ErrorId * DateTime * string) list
      LastNormalSize: float * float
      LastNormalLocation: int * int
      LastWindowState: WindowState
      WritePreferencesRequests: (WritePreferencesRequestId * WritePreferencesRequestSource) list
      PlaylistsState: Playlists.Model.State
      PlayerState: Player.Model.State }

[<Literal>]
let MIN_WIDTH = 800.

[<Literal>]
let MIN_HEIGHT = 600.

let private preferencesFile = $"{Environment.UserName}.{fileExtension Preferences}"

let defaultPreferences =
    { NormalSize = MIN_WIDTH, MIN_HEIGHT
      NormalLocation = 0, 0
      WindowState = WindowState.Normal
      Muted = false
      Volume = 100 }

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
