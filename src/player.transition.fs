module Aornota.Fap.Player.Transition

open Elmish
open Aornota.Fap.Domain
open Aornota.Fap.Player.State
open Aornota.Fap.Player.Utilities
open LibVLCSharp.Shared
open System.IO

type ExternalMsg =
    | Next
    | Previous
    | Play // TODO-NMB: Is this necessary?...
    | NotifyError of string

type Msg =
    | Play of TrackData
    | Seek of double
    | SetPos of int64
    | SetLength of int64
    | SetPlayState of bool
    | Previous
    | Pause
    | Stop
    | PlayInternal
    | Next

let init: State =
    { Length = 0L
      SliderPos = 0
      IsPlaying = false }

let update msg (state: State.State) (player: MediaPlayer) =
    match msg with
    | SetPlayState isPlaying -> { state with IsPlaying = isPlaying }, Cmd.none, None
    | Play track ->
        use media = getMediaFromlocal (Path.Combine(track.Folder, track.Name))
        player.Play media |> ignore

        let batch =
            Cmd.batch [ Cmd.ofMsg (SetLength player.Length); Cmd.ofMsg (SetPlayState true) ]

        state, batch, None
    | Seek position ->
        let time = (position |> int64) * player.Length / 100L
        // TODO-NMB: Find a way to differentiate from user action vs player event...
        state, Cmd.none, None
    | SetLength length -> { state with Length = length }, Cmd.none, None
    | SetPos position ->
        let pos = (position * 100L / player.Length) |> int
        { state with SliderPos = pos }, Cmd.none, None
    | Previous ->
        player.PreviousChapter() // TODO-NMB: Is this necessary?...
        state, Cmd.none, Some ExternalMsg.Previous
    | Next ->
        player.NextChapter() // TODO-NMB: Is this necessary?...
        state, Cmd.none, Some ExternalMsg.Next
    | Pause ->
        player.Pause()
        state, Cmd.ofMsg (SetPlayState false), None
    | Stop ->
        player.Stop()
        state, Cmd.ofMsg (SetPlayState false), None
    | PlayInternal -> state, Cmd.none, Some ExternalMsg.Play
