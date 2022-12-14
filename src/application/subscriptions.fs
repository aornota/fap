module Aornota.Fap.Application.Subscriptions

open Avalonia.FuncUI.Hosts
open Elmish
open LibVLCSharp.Shared
open System

let locationChanged (window: HostWindow) =
    let sub dispatch =
        window.PositionChanged.Subscribe(fun _ -> dispatch Transition.Msg.LocationChanged)
        |> ignore

    Cmd.ofSub sub

let effectiveViewportChanged (window: HostWindow) =
    let sub dispatch =
        window.EffectiveViewportChanged.Subscribe(fun _ -> dispatch Transition.Msg.EffectiveViewportChanged)
        |> ignore

    Cmd.ofSub sub

let playbackErrored (player: MediaPlayer) =
    let sub dispatch =
        player.EncounteredError.Subscribe(fun _ -> dispatch Transition.Msg.PlayerErrored)
        |> ignore

    Cmd.ofSub sub

let playing (player: MediaPlayer) =
    let sub dispatch =
        player.Playing.Subscribe(fun _ -> dispatch Transition.Msg.PlayerPlaying)
        |> ignore

    Cmd.ofSub sub

let positionChanged (player: MediaPlayer) =
    let sub dispatch =
        player.PositionChanged.Subscribe(fun args -> dispatch (Transition.Msg.PlayerPositionChanged args.Position))
        |> ignore

    Cmd.ofSub sub

let ended (player: MediaPlayer) =
    let sub dispatch =
        player.EndReached.Subscribe(fun _ -> dispatch Transition.Msg.PlayerEnded)
        |> ignore

    Cmd.ofSub sub
