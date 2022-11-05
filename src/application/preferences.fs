module Aornota.Fap.Application.Preferences

open Aornota.Fap.Application.Model
open Aornota.Fap.Literals.Application
open Aornota.Fap.Persistence
open Avalonia.Controls
open System

type Preferences =
    { NormalSize: float * float
      NormalLocation: int * int
      WindowState: WindowState
      LastSessionId: SessionId option
      AutoPlaySession: bool
      LastAudioFolder: string
      Muted: bool
      Volume: int }

let private preferencesFile =
    $"{Environment.UserName.ToLowerInvariant()}.{fileExtension Preferences}"

let defaultPreferences =
    { NormalSize = WINDOW_MINIMUM_WIDTH, WINDOW_MINIMUM_HEIGHT
      NormalLocation = 0, 0
      WindowState = WindowState.Normal
      LastSessionId = None
      AutoPlaySession = false
      LastAudioFolder = myMusicFolder
      Muted = false
      Volume = 100 }

let readPreferences () =
    async { return! read<Preferences> Preferences preferencesFile }

let writePreferences (preferences: Preferences) =
    async { return! write Preferences preferencesFile preferences }
