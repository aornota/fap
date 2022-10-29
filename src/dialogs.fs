module Aornota.Fap.Dialogs

(* open Aornota.Fap.Utilities
open Avalonia.Controls
open System.Collections.Generic

let filesDialog (filters: FileDialogFilter seq option) =
    let dialog = OpenFileDialog()

    let filters =
        match filters with
        | Some filter -> filter
        | None ->
            let filter = FileDialogFilter()

            filter.Extensions <- List(fileExtensions)

            filter.Name <- "Audio files"
            seq { filter }

    dialog.AllowMultiple <- true
    // TODO-NMB: Make configurable?...
    dialog.Directory <- "D:\\AUDIO"
    dialog.Title <- "Select file/s"
    dialog.Filters <- List(filters)
    dialog

let folderDialog () =
    let dialog = OpenFolderDialog()
    // TODO-NMB: Make configurable?...
    dialog.Directory <- "D:\\AUDIO"
    dialog.Title <- "Select folder"
    dialog *)
