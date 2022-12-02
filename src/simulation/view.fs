module Aornota.Fap.Simulation.View

open Aornota.Fap.Simulation.Transition
open Avalonia.Controls
open Avalonia.FuncUI.DSL
open Avalonia.Layout
open Avalonia.Media

let view (colour: string) state =
    Border.create
        [ Border.dock Dock.Right
          Border.verticalAlignment VerticalAlignment.Center
          Border.horizontalAlignment HorizontalAlignment.Center
          Border.borderBrush colour
          Border.borderThickness 1
          Border.margin (10, 10, 10, 10)
          Border.child (
              Image.create
                  [ Image.dock Dock.Right
                    Image.verticalAlignment VerticalAlignment.Center
                    Image.horizontalAlignment HorizontalAlignment.Center
                    Image.stretch Stretch.Fill
                    Image.source (asAvaloniaBitmap state) ]
          ) ]
