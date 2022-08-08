[<RequireQualifiedAccess>]
module Aornota.Fap.Player.View

open Aornota.Fap
open Aornota.Fap.Player
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.FuncUI.Types

let private mediaButtons (state: State.State) dispatch =
    let button (icon: IView<Canvas>) enabled onClick =
        Button.create
            [ Button.content icon
              Button.classes [ "mediabtn" ]
              Button.isEnabled enabled
              Button.onClick onClick ]

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Bottom
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Horizontal
          StackPanel.dock Dock.Top
          StackPanel.children
              [ button Icons.previous true (fun _ -> dispatch Transition.Msg.Previous) // TODO-NMB: Conditionally enable/disable?...

                if (state.IsPlaying) then
                    button Icons.pause true (fun _ -> dispatch Transition.Msg.Pause)
                else
                    button Icons.play true (fun _ -> dispatch Transition.Msg.PlayInternal)

                button Icons.stop state.IsPlaying (fun _ -> dispatch Transition.Msg.Stop)
                button Icons.next true (fun _ -> dispatch Transition.Msg.Next) ] ] // TODO-NMB: Conditionally enable/disable?...

let private progressBar (state: State.State) dispatch =
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
                      Slider.value (state.SliderPos |> double)
                      Slider.onValueChanged (fun value -> dispatch (Transition.Msg.Seek value)) ] ] ]

let view state dispatch =
    DockPanel.create
        [ DockPanel.classes [ "mediabar" ]
          DockPanel.dock Dock.Bottom
          DockPanel.horizontalAlignment HorizontalAlignment.Center
          DockPanel.children [ progressBar state dispatch; mediaButtons state dispatch ] ]
