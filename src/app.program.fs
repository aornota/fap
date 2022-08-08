module Aornota.Fap.App.Program

open Aornota.Fap
open Aornota.Fap.App
open Elmish
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.FuncUI
open Avalonia.FuncUI.Elmish
open Avalonia.FuncUI.Hosts
open Avalonia.Input
open Avalonia.Themes.Fluent
open LibVLCSharp.Shared

type AppWindow() as this =
    inherit HostWindow()

    do
        base.Title <- Transition.AppName
        base.Width <- 800.0
        base.Height <- 600.0
        base.MinWidth <- 526.0
        base.MinHeight <- 526.0
        this.SystemDecorations <- SystemDecorations.Full

        //this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        //this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        let player = Player.Utilities.getEmptyPlayer
        let init _ = Transition.init, Cmd.none
#if DEBUG
        this.AttachDevTools(KeyGesture(Key.F12))
#endif
        let updateWithServices (msg: Transition.Msg) (state: State.State) =
            Transition.transition msg state this player

        Program.mkProgram init updateWithServices View.view
        |> Program.withHost this
        |> Program.withSubscription (fun _ -> Subscriptions.playing player)
        |> Program.withSubscription (fun _ -> Subscriptions.paused player)
        |> Program.withSubscription (fun _ -> Subscriptions.stopped player)
        |> Program.withSubscription (fun _ -> Subscriptions.ended player)
        |> Program.withSubscription (fun _ -> Subscriptions.timeChanged player)
        |> Program.withSubscription (fun _ -> Subscriptions.lengthChanged player)
        |> Program.withSubscription (fun _ -> Subscriptions.chapterChanged player)
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> Program.run

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme(baseUri = null, Mode = FluentThemeMode.Dark))
        this.Styles.Load "avares://fap/styles/styles.xaml"
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
