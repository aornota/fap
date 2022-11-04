module Aornota.Fap.App.Program

open Aornota.Fap
open Aornota.Fap.App.Model
open Aornota.Fap.App.Preferences
open Aornota.Fap.App.Subscriptions
open Aornota.Fap.App.Transition
open Aornota.Fap.App.View
open Aornota.Fap.Literals
open Aornota.Fap.Persistence
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
open System

let private player =
    use libvlc = new LibVLC()
    new MediaPlayer(libvlc)

type AppWindow(preferences, session: Session, sessionIds, playlistIds, startupErrors) as this =
    inherit HostWindow()

    do
        base.Title <- $"{session.Name} | {applicationNameAndVersion}"

        base.Icon <- applicationIcon ICON_VARIANT_INACTIVE preferences.Muted
        base.MinWidth <- WINDOW_MINIMUM_WIDTH
        base.MinHeight <- WINDOW_MINIMUM_HEIGHT
        base.Width <- Math.Max(fst preferences.NormalSize, WINDOW_MINIMUM_WIDTH)
        base.Height <- Math.Max(snd preferences.NormalSize, WINDOW_MINIMUM_HEIGHT)
        base.Position <- PixelPoint(fst preferences.NormalLocation, snd preferences.NormalLocation)
        base.WindowState <- preferences.WindowState
        this.SystemDecorations <- SystemDecorations.Full

        // this.VisualRoot.VisualRoot.Renderer.DrawFps <- true
        // this.VisualRoot.VisualRoot.Renderer.DrawDirtyRects <- true

        player.Mute <- preferences.Muted
        player.Volume <- playerVolume preferences.Volume

        let init _ =
            init preferences session sessionIds playlistIds startupErrors player
#if DEBUG
        this.AttachDevTools(KeyGesture(Key.F12))
#endif
        let updateWithServices msg state = transition msg state this player

        Program.mkProgram init updateWithServices view
        |> Program.withHost this
        |> Program.withSubscription (fun _ -> locationChanged this)
        |> Program.withSubscription (fun _ -> effectiveViewportChanged this)
        |> Program.withSubscription (fun _ -> playbackErrored player)
        |> Program.withSubscription (fun _ -> playing player)
        |> Program.withSubscription (fun _ -> positionChanged player)
        |> Program.withSubscription (fun _ -> ended player)
#if DEBUG
        // |> Program.withConsoleTrace
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
                | Error error -> [ makeError error (Some "Unable to write default preferences") ]

            let preferences, startupErrors =
                match readPreferences () |> Async.RunSynchronously with
                | Ok preferences -> preferences, []
                | Error FileNotFound -> defaultPreferences, writeDefaultPreferences defaultPreferences
                | Error (Other error) ->
                    defaultPreferences,
                    writeDefaultPreferences defaultPreferences
                    @ [ makeError error (Some "Unable to read preferences") ]

            let writeDefaultSession defaultSession =
                match writeSession defaultSession |> Async.RunSynchronously with
                | Ok _ -> []
                | Error error -> [ makeError error (Some "Unable to write default session") ]

            let defaultSession = newSession ()

            let session, startupErrors =
                match preferences.LastSessionId with
                | Some sessionId ->
                    let (SessionId guid) = sessionId
                    let unableToRead = Some $"Unable to read last session ({guid})"

                    match readSession sessionId |> Async.RunSynchronously with
                    | Ok session -> session, startupErrors
                    | Error FileNotFound ->
                        defaultSession,
                        writeDefaultSession defaultSession
                        @ [ makeError $"File not found for {sessionId}" unableToRead ] @ startupErrors
                    | Error (Other error) ->
                        defaultSession,
                        writeDefaultSession defaultSession
                        @ [ makeError error unableToRead ] @ startupErrors
                | None -> defaultSession, startupErrors

            let sessionIds =
                listNamesWithoutExtension Session
                |> List.choose (fun name ->
                    match Guid.TryParse(name) with
                    | true, guid -> Some(SessionId guid)
                    | _ -> None)

            let playlistIds =
                listNamesWithoutExtension Playlist
                |> List.choose (fun name ->
                    match Guid.TryParse(name) with
                    | true, guid -> Some(Playlists.Model.PlaylistId guid)
                    | _ -> None)

            desktopLifetime.MainWindow <- AppWindow(preferences, session, sessionIds, playlistIds, startupErrors)
        | _ -> ()

[<EntryPoint>]
let main (args: string[]) =
    AppBuilder
        .Configure<App>()
        .UsePlatformDetect()
        .UseSkia()
        .StartWithClassicDesktopLifetime(args)
