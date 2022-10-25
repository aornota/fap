module Aornota.Fap.App.Program

open Aornota.Fap
open Aornota.Fap.App.Transition
open Aornota.Fap.App.View
open Aornota.Fap.Literals
open Aornota.Fap.Utilities
open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Input
open Avalonia.Themes.Fluent
open LibVLCSharp.Shared

type AppWindow() as this =
    inherit HostWindow()

    do
        base.Title <- APPLICATION_NAME
        base.Width <- 800.0
        base.Height <- 600.0
        base.MinWidth <- 800.0
        base.MinHeight <- 600.0
        this.SystemDecorations <- SystemDecorations.Full

        //?this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        //?this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        // TODO-NMB: Get from preferences?...
        let muted, volume = false, 100

        let player = Player.Utilities.getEmptyPlayer
        player.Mute <- muted
        player.Volume <- playerVolume volume
        let init _ = init muted volume, Cmd.none
#if DEBUG
        this.AttachDevTools(KeyGesture(Key.F12))
#endif
        let updateWithServices msg state = transition msg state this player

        Program.mkProgram init updateWithServices view
        |> Program.withHost this
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
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.MainWindow <- AppWindow()
        | _ -> ()

[<EntryPoint>]
let main (args: string[]) =
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .UseSkia()
        .StartWithClassicDesktopLifetime(args)
