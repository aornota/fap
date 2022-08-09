module Aornota.Fap.App.Subscriptions

open Aornota.Fap.App
open Elmish
open LibVLCSharp.Shared
open System

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

let timeChanged (player: MediaPlayer) =
    let sub dispatch =
        player.TimeChanged.Subscribe(fun args -> dispatch (Transition.Msg.TimeChanged args.Time))
        |> ignore

    Cmd.ofSub sub

let chapterChanged (player: MediaPlayer) =
    let sub dispatch =
        player.ChapterChanged.Subscribe(fun args -> dispatch (Transition.Msg.ChapterChanged args.Chapter))
        |> ignore

    Cmd.ofSub sub

let lengthChanged (player: MediaPlayer) =
    let sub dispatch =
        player.LengthChanged.Subscribe(fun args -> dispatch (Transition.Msg.LengthChanged args.Length))
        |> ignore

    Cmd.ofSub sub
