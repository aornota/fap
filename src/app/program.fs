module Aornota.Fap.App.Program

open Aornota.Fap
open Aornota.Fap.App.Model
open Aornota.Fap.App.Transition
open Aornota.Fap.App.View
open Aornota.Fap.Domain
open Aornota.Fap.Persistence
open Aornota.Fap.Utilities
open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Input
open Avalonia.Media.Imaging
open Avalonia.Themes.Fluent
open LibVLCSharp.Shared
open System

type AppWindow(preferences: Preferences, preferencesErrors) as this =
    inherit HostWindow()

    do
        base.Title <- applicationNameAndVersion

        base.Icon <- applicationIcon Inactive preferences.Muted
        base.MinWidth <- MIN_WIDTH
        base.MinHeight <- MIN_HEIGHT
        base.Width <- Math.Max(fst preferences.NormalSize, MIN_WIDTH)
        base.Height <- Math.Max(snd preferences.NormalSize, MIN_HEIGHT)
        base.Position <- PixelPoint(fst preferences.NormalLocation, snd preferences.NormalLocation)
        base.WindowState <- preferences.WindowState
        this.SystemDecorations <- SystemDecorations.Full

        // this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        // this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        let player = Player.Utilities.getEmptyPlayer
        player.Mute <- preferences.Muted
        player.Volume <- playerVolume preferences.Volume

        let init _ = init preferences preferencesErrors
#if DEBUG
        this.AttachDevTools(KeyGesture(Key.F12))
#endif
        let updateWithServices msg state = transition msg state this player

        Program.mkProgram init updateWithServices view
        |> Program.withHost this
        |> Program.withSubscription (fun _ -> Subscriptions.locationChanged this)
        |> Program.withSubscription (fun _ -> Subscriptions.effectiveViewportChanged this)
        |> Program.withSubscription (fun _ -> Subscriptions.playing player)
        |> Program.withSubscription (fun _ -> Subscriptions.paused player)
        |> Program.withSubscription (fun _ -> Subscriptions.stopped player)
        |> Program.withSubscription (fun _ -> Subscriptions.ended player)
        |> Program.withSubscription (fun _ -> Subscriptions.positionChanged player)
        |> Program.withSubscription (fun _ -> Subscriptions.playbackErrored player)
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme(baseUri = null, Mode = FluentThemeMode.Dark))
        Core.Initialize()

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            let writeDefaultPreferences defaultPreferences =
                match writePreferences defaultPreferences |> Async.RunSynchronously with
                | Ok _ -> []
                | Error error -> [ $"Program.writeDefaultPreferences -> {error}" ]

            let preferences, preferencesErrors =
                match readPreferences () |> Async.RunSynchronously with
                | Ok preferences -> preferences, []
                | Error FileNotFound -> defaultPreferences, writeDefaultPreferences defaultPreferences
                | Error (Other error) -> defaultPreferences, writeDefaultPreferences defaultPreferences @ [ error ]

            desktopLifetime.MainWindow <- AppWindow(preferences, preferencesErrors)
        | _ -> ()

[<EntryPoint>]
// TODO-NMB: How to make use of args?...
let main (args: string[]) =
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .UseSkia()
        .StartWithClassicDesktopLifetime(args)
