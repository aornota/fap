module Aornota.Fap.Dialogs

open Aornota.Fap.Literals
open Aornota.Fap.Utilities
open Avalonia.Controls
open System.Collections.Generic

let filesDialog folder =
    let filter = FileDialogFilter()

    filter.Extensions <- List(fileExtensions)
    filter.Name <- "Audio files"

    let dialog = OpenFileDialog()

    dialog.Directory <- folder
    dialog.Title <- ADD_FILES
    dialog.AllowMultiple <- true
    dialog.Filters <- List(seq { filter })
    dialog

let folderDialog folder =
    let dialog = OpenFolderDialog()
    dialog.Directory <- folder
    dialog.Title <- ADD_FOLDER
    dialog
