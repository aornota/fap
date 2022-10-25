module Aornota.Fap.Player.View

open Aornota.Fap
open Aornota.Fap.Literals
open Aornota.Fap.Player.Model
open Aornota.Fap.Player.Transition
open Aornota.Fap.Utilities
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media

[<Literal>]
let private NO_SELECTED_TRACK = "- no track selected -"

let private colour state =
    match state with
    | Some trackState ->
        match trackState.PlayerState with
        | NoMedia
        | Stopped _
        | Ended -> COLOUR_INACTIVE
        | AwaitingPlay _
        | Paused _ -> COLOUR_AWAITING
        | Playing _ -> COLOUR_ACTIVE
        | PlaybackErrored -> COLOUR_ERROR
    | None -> COLOUR_DISABLED_TEXT

let private progressBar state dispatch =
    let colour = colour state

    let enabled, positionValue, duration =
        match state with
        | Some trackState ->
            let enabled, position =
                match trackState.PlayerState with
                | Playing (position, _)
                | Paused position
                | Stopped position -> true, position
                | _ -> false, START_POSITION

            enabled, position, trackState.Track.Duration
        | None -> false, START_POSITION, None

    let position =
        TextBlock.create
            [ TextBlock.verticalAlignment VerticalAlignment.Center
              TextBlock.textAlignment TextAlignment.Right
              TextBlock.width 40.
              TextBlock.fontSize 12.
              TextBlock.foreground colour
              TextBlock.text (positionText positionValue duration) ]

    let slider =
        Slider.create
            [ Slider.horizontalAlignment HorizontalAlignment.Center
              Slider.width 500.
              Slider.minimum 0.
              Slider.maximum 100.
              Slider.foreground colour
              Slider.isEnabled enabled
              Slider.value (positionValue * 100f |> double)
              Slider.tip "Seek within track"
              Slider.onValueChanged (fun value -> dispatch (Seek(value / 100. |> float32))) ]

    let duration =
        TextBlock.create
            [ TextBlock.verticalAlignment VerticalAlignment.Center
              TextBlock.textAlignment TextAlignment.Left
              TextBlock.width 40.
              TextBlock.fontSize 12.
              TextBlock.foreground colour
              TextBlock.text (durationText RoundUp duration) ]

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Center
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Horizontal
          StackPanel.spacing 10.
          StackPanel.children [ position; slider; duration ] ]

let private trackDetails state =
    let colour = colour state

    // TODO-NMB: Use "metadata" (if available)?...
    let details =
        match state with
        | Some trackState -> $"{trackState.Track.Name} | {trackState.PlaylistName}"
        | None -> NO_SELECTED_TRACK

    TextBlock.create
        [ TextBlock.horizontalAlignment HorizontalAlignment.Center
          TextBlock.fontSize 12
          TextBlock.foreground colour
          TextBlock.text details ]

let private media state dispatch =
    let button
        (fIcon: bool -> string option -> string option -> IView<Canvas>)
        enabled
        enabledColourOverride
        disabledColourOverride
        leftMargin
        (tip: string)
        onClick
        =
        Button.create
            [ Button.width SIZE_BUTTON_WITH_ICON
              Button.height SIZE_BUTTON_WITH_ICON
              Button.background COLOUR_BACKGROUND
              Button.margin (leftMargin, 0, 0, 0)
              Button.cornerRadius 0
              Button.isEnabled enabled
              Button.content (fIcon enabled enabledColourOverride disabledColourOverride)
              Button.tip tip
              Button.onClick onClick ]

    let isPlayingOrAwaitingPlay, allowPrevious, allowNext, allowPlay, playDisabledColourOverride, allowPause, allowStop =
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | NoMedia -> false, trackState.HasPrevious, trackState.HasNext, false, None, false, false
            | AwaitingPlay _ ->
                true, trackState.HasPrevious, trackState.HasNext, false, Some COLOUR_AWAITING, false, false
            | PlaybackErrored ->
                false, trackState.HasPrevious, trackState.HasNext, false, Some COLOUR_ERROR, false, false
            | Playing _ -> true, trackState.HasPrevious, trackState.HasNext, false, None, true, true
            | Paused _ -> false, trackState.HasPrevious, trackState.HasNext, true, None, false, true
            | Stopped _
            | Ended -> false, trackState.HasPrevious, trackState.HasNext, true, None, false, false
        | None -> false, false, false, false, None, false, false

    let previousAndNextEnabledColourOverride =
        if isPlayingOrAwaitingPlay then
            COLOUR_ACTIVE
        else
            COLOUR_INACTIVE

    let muteOrUnmuteIcon, muteOrUnmuteTip =
        if state.Muted then
            Icons.muted, "Unmute"
        else
            let icon =
                match state.Volume with
                | volume when volume < 34 -> Icons.unmutedLow
                | volume when volume < 67 -> Icons.unmutedMedium
                | _ -> Icons.unmutedHigh

            icon, "Mute"

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Center
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Horizontal
          StackPanel.margin (0, 15, 0, 15)
          StackPanel.children
              [ button
                    Icons.previous
                    allowPrevious
                    (Some previousAndNextEnabledColourOverride)
                    None
                    0
                    "Previous track"
                    (fun _ -> dispatch Previous)

                if allowPause then
                    button Icons.pause true (Some COLOUR_AWAITING) None 0 "Pause track" (fun _ -> dispatch Pause)
                else
                    button Icons.play allowPlay None playDisabledColourOverride 0 "Play track" (fun _ -> dispatch Play)

                button Icons.stop allowStop (Some COLOUR_INACTIVE) None 0 "Stop track" (fun _ -> dispatch Stop)
                button Icons.next allowNext (Some previousAndNextEnabledColourOverride) None 0 "Next track" (fun _ ->
                    dispatch Next)
                button
                    muteOrUnmuteIcon
                    (state.Volume <> 0)
                    (Some COLOUR_VOLUME)
                    (Some COLOUR_VOLUME)
                    20
                    muteOrUnmuteTip
                    (fun _ -> dispatch ToggleMuted)
                Slider.create
                    [ Slider.verticalAlignment VerticalAlignment.Center
                      Slider.horizontalAlignment HorizontalAlignment.Center
                      Slider.width 100.
                      Slider.minimum 0.
                      Slider.maximum 100.
                      Slider.margin (8, 0, 0, 0)
                      Slider.padding (0, 0, 0, 6)
                      Slider.foreground COLOUR_VOLUME
                      Slider.value state.Volume
                      Slider.tip $"Volume: {state.Volume}%%"
                      Slider.onValueChanged (fun value -> dispatch (Volume(value |> int))) ] ] ]

let view state dispatch =
    StackPanel.create
        [ StackPanel.dock Dock.Bottom
          StackPanel.verticalAlignment VerticalAlignment.Center
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Vertical
          StackPanel.children
              [ progressBar state.TrackState dispatch
                trackDetails state.TrackState
                media state dispatch ] ]
