[<RequireQualifiedAccess>]
module Aornota.Fap.Icons

// See https://materialdesignicons.com/.

open Aornota.Fap.Literals
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.DSL

let private create (data: string) enabled enabledColourOverride disabledColourOverride =
    let colour =
        match enabled, enabledColourOverride, disabledColourOverride with
        | true, Some enabledColourOverride, _ -> enabledColourOverride
        | true, None, _ -> COLOUR_ACTIVE
        | false, _, Some disabledColourOverride -> disabledColourOverride
        | false, _, None -> COLOUR_DISABLED_ICON

    Canvas.create
        [ Canvas.width 24.0
          Canvas.height 24.0
          Canvas.children [ Path.create [ Path.fill colour; Path.data data ] ] ]

let previous enabled =
    create "M6,18V6H8V18H6M9.5,12L18,6V18L9.5,12Z" enabled

let play enabled =
    create "M8,5.14V19.14L19,12.14L8,5.14Z" enabled

let pause enabled =
    create "M14,19H18V5H14M6,19H10V5H6V19Z" enabled

let stop enabled = create "M18,18H6V6H18V18Z" enabled

let next enabled =
    create "M16,18H18V6H16M6,18L14.5,12L6,6V18Z" enabled
