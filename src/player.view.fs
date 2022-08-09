module Aornota.Fap.Player.View

open Aornota.Fap
open Aornota.Fap.Player.State
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

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Top
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Horizontal
          StackPanel.dock Dock.Top
          StackPanel.children
              [ button Icons.previous false (fun _ -> dispatch Previous) // TODO-NMB: Conditionally enable/disable...

                if (state.IsPlaying) then
                    button Icons.pause true (fun _ -> dispatch Pause)
                else
                    button Icons.play true (fun _ -> dispatch PlayInternal) // TODO-NMB: Conditionally enable/disable...

                button Icons.stop state.IsPlaying (fun _ -> dispatch Stop)
                button Icons.next false (fun _ -> dispatch Next) ] ] // TODO-NMB: Conditionally enable/disable...

let private progressBar state dispatch =
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
                      Slider.isEnabled state.IsPlaying
                      Slider.value (state.SliderPos |> double)
                      Slider.onValueChanged (fun value -> dispatch (Seek value)) ] ] ]

let view state dispatch =
    DockPanel.create
        [ DockPanel.classes [ "mediabar" ]
          DockPanel.dock Dock.Bottom
          DockPanel.horizontalAlignment HorizontalAlignment.Center
          DockPanel.children [ mediaButtons state dispatch; progressBar state dispatch ] ]
