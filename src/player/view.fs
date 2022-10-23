module Aornota.Fap.Player.View

open Aornota.Fap
open Aornota.Fap.Player.Model
open Aornota.Fap.Player.Transition
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.FuncUI.Types

let private mediaButtons state dispatch =
    let button (fIcon: bool -> IView<Canvas>) enabled onClick =
        Button.create
            [ Button.content (fIcon enabled)
              Button.width 48
              Button.cornerRadius 0
              Button.isEnabled enabled
              Button.onClick onClick ]

    let allowPrevious, allowNext, allowPlay, allowPause, allowStop =
        match state with
        | Some trackState ->
            match trackState.PlayerState with
            | NotLoaded -> trackState.HasPrevious, trackState.HasNext, false, false, false
            | NotPlaying -> trackState.HasPrevious, trackState.HasNext, true, false, false
            | Playing _ -> trackState.HasPrevious, trackState.HasNext, false, true, true
            | Paused _ -> trackState.HasPrevious, trackState.HasNext, true, false, true
        | None -> false, false, false, false, false

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Top
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Horizontal
          StackPanel.dock Dock.Top
          StackPanel.children
              [ button Icons.previous allowPrevious (fun _ -> dispatch RequestPrevious)

                if allowPause then
                    button Icons.pause true (fun _ -> dispatch RequestPause)
                else
                    button Icons.play allowPlay (fun _ -> dispatch RequestPlay)

                button Icons.stop allowStop (fun _ -> dispatch RequestStop)
                button Icons.next allowNext (fun _ -> dispatch RequestNext) ] ]

let private progressBar state dispatch =
    // TODO-NMB: Should slider be enabled when Paused (&c.)?...
    let enabled, position =
        match state with
        | Some trackState ->
            match trackState.PlayerState with
            | Playing (position, _) -> true, position
            | Paused position -> false, position
            | _ -> false, START_POSITION
        | None -> false, START_POSITION

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Bottom
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Horizontal
          StackPanel.dock Dock.Bottom
          StackPanel.children
              [ Slider.create
                    [ Slider.minimum 0.0
                      Slider.maximum 100.0
                      Slider.width 400.0
                      Slider.horizontalAlignment HorizontalAlignment.Center
                      Slider.isEnabled enabled
                      Slider.value (position * 100f |> double)
                      Slider.onValueChanged (fun value -> dispatch (RequestSeek value)) ] ] ]

let view state dispatch =
    DockPanel.create
        [ DockPanel.classes [ "mediabar" ]
          DockPanel.dock Dock.Bottom
          DockPanel.horizontalAlignment HorizontalAlignment.Center
          DockPanel.children [ mediaButtons state dispatch; progressBar state dispatch ] ]
