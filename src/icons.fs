[<RequireQualifiedAccess>]
module Aornota.Fap.Icons

// See https://materialdesignicons.com/.

open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.DSL

[<Literal>]
let private EnabledColour = "cyan"

[<Literal>]
let private DisabledColour = "black"

let stop enabled =
    Canvas.create
        [ Canvas.width 24.0
          Canvas.height 24.0
          Canvas.children
              [ Path.create
                    [ Path.fill (if enabled then EnabledColour else DisabledColour)
                      Path.data "M18,18H6V6H18V18Z" ] ] ]

let play enabled =
    Canvas.create
        [ Canvas.width 24.0
          Canvas.height 24.0
          Canvas.children
              [ Path.create
                    [ Path.fill (if enabled then EnabledColour else DisabledColour)
                      Path.data "M8,5.14V19.14L19,12.14L8,5.14Z" ] ] ]

let pause enabled =
    Canvas.create
        [ Canvas.width 24.0
          Canvas.height 24.0
          Canvas.children
              [ Path.create
                    [ Path.fill (if enabled then EnabledColour else DisabledColour)
                      Path.data "M14,19H18V5H14M6,19H10V5H6V19Z" ] ] ]

let previous enabled =
    Canvas.create
        [ Canvas.width 24.0
          Canvas.height 24.0
          Canvas.children
              [ Path.create
                    [ Path.fill (if enabled then EnabledColour else DisabledColour)
                      Path.data "M6,18V6H8V18H6M9.5,12L18,6V18L9.5,12Z" ] ] ]

let next enabled =
    Canvas.create
        [ Canvas.width 24.0
          Canvas.height 24.0
          Canvas.children
              [ Path.create
                    [ Path.fill (if enabled then EnabledColour else DisabledColour)
                      Path.data "M16,18H18V6H16M6,18L14.5,12L6,6V18Z" ] ] ]
