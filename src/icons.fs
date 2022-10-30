[<RequireQualifiedAccess>]
module Aornota.Fap.Icons

// See https://materialdesignicons.com/.

open Aornota.Fap.Literals
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.FuncUI.DSL

[<Literal>]
let private SIZE_CANVAS = 24.

let private create (pathData: string) enabled enabledColourOverride disabledColourOverride =
    let colour =
        match enabled, enabledColourOverride, disabledColourOverride with
        | true, Some enabledColourOverride, _ -> enabledColourOverride
        | true, None, _ -> COLOUR_ACTIVE
        | false, _, Some disabledColourOverride -> disabledColourOverride
        | false, _, None -> COLOUR_DISABLED_ICON

    Canvas.create
        [ Canvas.width SIZE_CANVAS
          Canvas.height SIZE_CANVAS
          Canvas.children [ Path.create [ Path.fill colour; Path.data pathData ] ] ]

// #region Player icons
let previous = create "M6,18V6H8V18H6M9.5,12L18,6V18L9.5,12Z"

let play = create "M8,5.14V19.14L19,12.14L8,5.14Z"

let pause = create "M14,19H18V5H14M6,19H10V5H6V19Z"

let stop = create "M18,18H6V6H18V18Z"

let next = create "M16,18H18V6H16M6,18L14.5,12L6,6V18Z"

let muted =
    create
        "M5.64,3.64L21.36,19.36L19.95,20.78L16,16.83V20L11,15H7V9H8.17L4.22,5.05L5.64,3.64M16,4V11.17L12.41,7.58L16,4Z"

let unmutedLow = create "M7,9V15H11L16,20V4L11,9H7Z"

let unmutedMedium =
    create "M5,9V15H9L14,20V4L9,9M18.5,12C18.5,10.23 17.5,8.71 16,7.97V16C17.5,15.29 18.5,13.76 18.5,12Z"

let unmutedHigh =
    create
        "M14,3.23V5.29C16.89,6.15 19,8.83 19,12C19,15.17 16.89,17.84 14,18.7V20.77C18,19.86 21,16.28 21,12C21,7.72 18,4.14 14,3.23M16.5,12C16.5,10.23 15.5,8.71 14,7.97V16C15.5,15.29 16.5,13.76 16.5,12M3,9V15H7L12,20V4L7,9H3Z"
// #endregion

// #region Miscellaneous icons
let remove =
    create
        "M19,6.41L17.59,5L12,10.59L6.41,5L5,6.41L10.59,12L5,17.59L6.41,19L12,13.41L17.59,19L19,17.59L13.41,12L19,6.41Z"

let left = create "M14,7L9,12L14,17V7Z"

let right = create "M10,17L15,12L10,7V17Z"

let up = create "M7,15L12,10L17,15H7Z"

let down = create "M7,10L12,15L17,10H7Z"

let addAbove =
    create
        "M22,14A2,2 0 0,0 20,12H4A2,2 0 0,0 2,14V21H4V19H8V21H10V19H14V21H16V19H20V21H22V14M4,14H8V17H4V14M10,14H14V17H10V14M20,14V17H16V14H20M11,10H13V7H16V5H13V2H11V5H8V7H11V10Z"

let addBelow =
    create
        "M22,10A2,2 0 0,1 20,12H4A2,2 0 0,1 2,10V3H4V5H8V3H10V5H14V3H16V5H20V3H22V10M4,10H8V7H4V10M10,10H14V7H10V10M20,10V7H16V10H20M11,14H13V17H16V19H13V22H11V19H8V17H11V14Z"
// #endregion
