module Aornota.Fap.App.Subscriptions

open Aornota.Fap.App
open Avalonia.FuncUI.Hosts
open Elmish
open LibVLCSharp.Shared
open System

// #region HostWindow subscription
let locationChanged (window: HostWindow) =
    let sub dispatch =
        window.PositionChanged.Subscribe(fun _ -> dispatch (Transition.Msg.SavePreferencesApp))
        |> ignore

    Cmd.ofSub sub

let effectiveViewportChanged (window: HostWindow) =
    let sub dispatch =
        window.EffectiveViewportChanged.Subscribe(fun _ -> dispatch (Transition.Msg.SavePreferencesApp))
        |> ignore

    Cmd.ofSub sub
// #endregion

// #region MediaPlayer subscription
let playing (player: MediaPlayer) =
    let sub dispatch =
        player.Playing.Subscribe(fun _ -> dispatch Transition.Msg.Playing) |> ignore

    Cmd.ofSub sub

let paused (player: MediaPlayer) =
    let sub dispatch =
        player.Paused.Subscribe(fun _ -> dispatch Transition.Msg.Paused) |> ignore

    Cmd.ofSub sub

let stopped (player: MediaPlayer) =
    let sub dispatch =
        player.Stopped.Subscribe(fun _ -> dispatch Transition.Msg.Stopped) |> ignore

    Cmd.ofSub sub

let ended (player: MediaPlayer) =
    let sub dispatch =
        player.EndReached.Subscribe(fun _ -> dispatch Transition.Msg.Ended) |> ignore

    Cmd.ofSub sub

let positionChanged (player: MediaPlayer) =
    let sub dispatch =
        player.PositionChanged.Subscribe(fun args -> dispatch (Transition.Msg.PositionChanged args.Position))
        |> ignore

    Cmd.ofSub sub

let playbackErrored (player: MediaPlayer) =
    let sub dispatch =
        player.EncounteredError.Subscribe(fun _ -> dispatch Transition.Msg.PlaybackErrored)
        |> ignore

    Cmd.ofSub sub
// #endregion
