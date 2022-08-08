module Aornota.Fap.Dialogs

open Aornota.Fap.Domain
open Avalonia.Controls
open System.Collections.Generic

let getFilesDialog (filters: FileDialogFilter seq option) =
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
    dialog.Directory <- "D:\\AUDIO" // TODO-NMB: Make configurable?...
    dialog.Title <- "Select file/s"
    dialog.Filters <- List(filters)
    dialog

let getFolderDialog () =
    let dialog = OpenFolderDialog()
    dialog.Directory <- "D:\\AUDIO" // TODO-NMB: Make configurable?...
    dialog.Title <- "Select folder"
    dialog
