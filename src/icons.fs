[<RequireQualifiedAccess>]
module Aornota.Fap.Icons

// See https://materialdesignicons.com/.

open Aornota.Fap.Literals
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.DSL

[<Literal>]
let private SIZE_CANVAS = 24.

let private create (data: string) enabled enabledColourOverride disabledColourOverride =
    let colour =
        match enabled, enabledColourOverride, disabledColourOverride with
        | true, Some enabledColourOverride, _ -> enabledColourOverride
        | true, None, _ -> COLOUR_ACTIVE
        | false, _, Some disabledColourOverride -> disabledColourOverride
        | false, _, None -> COLOUR_DISABLED_ICON

    Canvas.create
        [ Canvas.width SIZE_CANVAS
          Canvas.height SIZE_CANVAS
          Canvas.children [ Path.create [ Path.fill colour; Path.data data ] ] ]

// #region Player icons
let previous = create "M6,18V6H8V18H6M9.5,12L18,6V18L9.5,12Z"

let play = create "M8,5.14V19.14L19,12.14L8,5.14Z"

let pause = create "M14,19H18V5H14M6,19H10V5H6V19Z"

let stop = create "M18,18H6V6H18V18Z"

let next = create "M16,18H18V6H16M6,18L14.5,12L6,6V18Z"

let muted =
    create
        "M3,9H7L12,4V20L7,15H3V9M16.59,12L14,9.41L15.41,8L18,10.59L20.59,8L22,9.41L19.41,12L22,14.59L20.59,16L18,13.41L15.41,16L14,14.59L16.59,12Z"

let unmuted =
    create "M5,9V15H9L14,20V4L9,9M18.5,12C18.5,10.23 17.5,8.71 16,7.97V16C17.5,15.29 18.5,13.76 18.5,12Z"
// #endregion

// #region Miscellaneous icons
let clear =
    create
        "M19,3H5A2,2 0 0,0 3,5V19A2,2 0 0,0 5,21H19A2,2 0 0,0 21,19V5A2,2 0 0,0 19,3M19,19H5V5H19V19M17,8.4L13.4,12L17,15.6L15.6,17L12,13.4L8.4,17L7,15.6L10.6,12L7,8.4L8.4,7L12,10.6L15.6,7L17,8.4Z"
// #endregion
