module Aornota.Fap.App.Model

open Aornota.Fap
open Aornota.Fap.Persistence
open Avalonia.Controls
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

type SavePreferencesRequestId =
    | SavePreferencesRequestId of Guid

    static member Create() =
        SavePreferencesRequestId(Guid.NewGuid())

type State =
    { ShowingErrors: bool
      Errors: (ErrorId * DateTime * string) list
      LastNormalSize: float * float
      LastNormalLocation: int * int
      LastWindowState: WindowState
      SavePreferencesAppRequestIds: SavePreferencesRequestId list
      SavePreferencesPlayerRequestIds: SavePreferencesRequestId list
      PlayerState: Player.Model.State
      PlaylistsState: Playlists.Model.State }

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

let writePreferences (preferences: Preferences) =
    async { return! write Preferences preferencesFile preferences }

let readPreferences () =
    async { return! read<Preferences> Preferences preferencesFile }
