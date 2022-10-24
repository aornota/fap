module Aornota.Fap.Player.View

open Aornota.Fap
open Aornota.Fap.Literals
open Aornota.Fap.Player.Model
open Aornota.Fap.Player.Transition
open Aornota.Fap.Utilities
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.FuncUI.Types

[<Literal>]
let private NO_SELECTED_TRACK = "- no selected track -"

// TODO-NMB: Mute/volume controls?...

let private progressBarAndTrackDetails state dispatch =
    let textColour =
        match state with
        | Some trackState ->
            match trackState.PlayerState with
            | NoMedia
            | Stopped
            | Ended -> COLOUR_INACTIVE
            | AwaitingPlay
            | Paused _ -> COLOUR_AWAITING
            | Playing _ -> COLOUR_ACTIVE
            | PlaybackErrored -> COLOUR_ERROR
        | None -> COLOUR_DISABLED_TEXT

    let progressBar =
        // TODO-NMB: Should slider be enabled when Paused | Stopped | &c.?...
        let enabled, positionValue, duration =
            match state with
            | Some trackState ->
                let enabled, position =
                    match trackState.PlayerState with
                    | Playing (position, _) -> true, position
                    | Paused position -> false, position
                    | _ -> false, START_POSITION

                enabled, position, trackState.Track.Duration
            | None -> false, START_POSITION, None

        let position =
            TextBlock.create
                [ TextBlock.verticalAlignment VerticalAlignment.Center
                  TextBlock.horizontalAlignment HorizontalAlignment.Center
                  TextBlock.width 40.
                  TextBlock.margin (0, 0, 10, 0)
                  TextBlock.fontSize 12.
                  TextBlock.foreground textColour
                  TextBlock.text (positionText positionValue duration) ]

        let slider =
            Slider.create
                [ Slider.horizontalAlignment HorizontalAlignment.Center
                  Slider.width 500.
                  Slider.minimum 0.
                  Slider.maximum 100.
                  Slider.isEnabled enabled
                  Slider.value (positionValue * 100f |> double)
                  Slider.onValueChanged (fun value -> dispatch (RequestSeek(value / 100. |> float32))) ]

        let duration =
            TextBlock.create
                [ TextBlock.verticalAlignment VerticalAlignment.Center
                  TextBlock.horizontalAlignment HorizontalAlignment.Center
                  TextBlock.width 40.
                  TextBlock.margin (10, 0, 0, 0)
                  TextBlock.fontSize 12.
                  TextBlock.foreground textColour
                  TextBlock.text (durationText duration) ]

        StackPanel.create
            [ StackPanel.verticalAlignment VerticalAlignment.Center
              StackPanel.horizontalAlignment HorizontalAlignment.Center
              StackPanel.orientation Orientation.Horizontal
              StackPanel.children [ position; slider; duration ] ]

    let trackDetails =
        let details =
            match state with
            | Some trackState -> $"{trackState.Track.Name} | {trackState.PlaylistName}"
            | None -> NO_SELECTED_TRACK

        TextBlock.create
            [ TextBlock.horizontalAlignment HorizontalAlignment.Center
              TextBlock.fontSize 12
              TextBlock.foreground textColour
              TextBlock.text details ]

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Center
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Vertical
          StackPanel.dock Dock.Top
          StackPanel.children [ progressBar; trackDetails ] ]

let private media state dispatch =
    let button
        (fIcon: bool -> string option -> string option -> IView<Canvas>)
        enabled
        enabledColourOverride
        disabledColourOverride
        onClick
        =
        Button.create
            [ Button.content (fIcon enabled enabledColourOverride disabledColourOverride)
              Button.width 48.
              Button.cornerRadius 0
              Button.isEnabled enabled
              Button.onClick onClick ]

    let isPlayingOrAwaitingPlay, allowPrevious, allowNext, allowPlay, playDisabledColourOverride, allowPause, allowStop =
        match state with
        | Some trackState ->
            match trackState.PlayerState with
            | NoMedia -> false, trackState.HasPrevious, trackState.HasNext, false, None, false, false
            | AwaitingPlay ->
                true, trackState.HasPrevious, trackState.HasNext, false, Some COLOUR_AWAITING, false, false
            | PlaybackErrored ->
                false, trackState.HasPrevious, trackState.HasNext, false, Some COLOUR_ERROR, false, false
            | Playing _ -> true, trackState.HasPrevious, trackState.HasNext, false, None, true, true
            | Paused _ -> false, trackState.HasPrevious, trackState.HasNext, true, None, false, true
            | Stopped
            | Ended -> false, trackState.HasPrevious, trackState.HasNext, true, None, false, false
        | None -> false, false, false, false, None, false, false

    let previousAndNextEnabledColourOverride =
        if isPlayingOrAwaitingPlay then
            COLOUR_ACTIVE
        else
            COLOUR_INACTIVE

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Top
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Horizontal
          StackPanel.dock Dock.Bottom
          StackPanel.margin (0, 15, 0, 15)
          StackPanel.children
              [ button Icons.previous allowPrevious (Some previousAndNextEnabledColourOverride) None (fun _ ->
                    dispatch RequestPrevious)

                if allowPause then
                    button Icons.pause true (Some COLOUR_AWAITING) None (fun _ -> dispatch RequestPause)
                else
                    button Icons.play allowPlay None playDisabledColourOverride (fun _ -> dispatch RequestPlay)

                button Icons.stop allowStop (Some COLOUR_INACTIVE) None (fun _ -> dispatch RequestStop)
                button Icons.next allowNext (Some previousAndNextEnabledColourOverride) None (fun _ ->
                    dispatch RequestNext) ] ]

let view state dispatch =
    DockPanel.create
        [ DockPanel.dock Dock.Bottom
          DockPanel.horizontalAlignment HorizontalAlignment.Center
          DockPanel.children
              [ progressBarAndTrackDetails state.TrackState dispatch
                media state.TrackState dispatch ] ]
