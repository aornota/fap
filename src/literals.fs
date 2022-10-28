module Aornota.Fap.Literals

[<Literal>]
let APPLICATION_NAME = "fap"

// (pre-α | α) | *β* | γ | δ | ε | ζ | η | θ | ι | κ | λ | μ | ν | ξ | ο | π | ρ) | σ | τ | υ | φ | χ | ψ | ω
[<Literal>]
let APPLICATION_VERSION = "β" // note: also update ..\README.md

// #region Colours
[<Literal>]
let COLOUR_ACTIVE = "#00Ffff" // cyan

[<Literal>]
let COLOUR_INACTIVE = "#ffffff" // white

[<Literal>]
let COLOUR_AWAITING = "#ffff00" // yellow

[<Literal>]
let COLOUR_BACKGROUND = "#000000" // black

[<Literal>]
let COLOUR_VOLUME = "#ee82ee" // violet

[<Literal>]
let COLOUR_ERROR = "#ff6347" // tomato

[<Literal>]
let COLOUR_SUMMARY = "#b0e0e6" // powderblue

[<Literal>]
let COLOUR_REMOVE = "#ffa07a" // lightsalmon

[<Literal>]
let COLOUR_DEBUG = "#dda0dd" // plum

[<Literal>]
let COLOUR_DISABLED_ICON = "#404040"

[<Literal>]
let COLOUR_DISABLED_TEXT = "#c0c0c0" // silver
// #endregion

// #region Folders
[<Literal>]
let FOLDER_SESSIONS = "sessions"

[<Literal>]
let FOLDER_PLAYLISTS = "playlists"
// #endregion

// #region File extensions
[<Literal>]
let FILE_EXTENSION_PREFERENCES = "fapper"

[<Literal>]
let FILE_EXTENSION_SESSION = "faps"

[<Literal>]
let FILE_EXTENSION_PLAYLIST = "fappy"

[<Literal>]
let FILE_EXTENSION_FLAC = "flac"

[<Literal>]
let FILE_EXTENSION_MP3 = "mp3"

[<Literal>]
let FILE_EXTENSION_WAV = "wav"
// #endregion

// #region Sizes
[<Literal>]
let SIZE_BUTTON_WITH_ICON = 30.
// #endregion
