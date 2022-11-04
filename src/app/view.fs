module Aornota.Fap.App.View

open Aornota.Fap
open Aornota.Fap.App.Model
open Aornota.Fap.App.Transition
open Aornota.Fap.Literals
open Aornota.Fap.Utilities
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media
open System

let private menu state dispatch =
    let separator = "-"

    let sessionMenu =
        let canChangeOrDeleteSession =
            match state.WriteSessionRequests, state.WritePreferencesRequests with
            | [], [] -> true
            | _ -> false

        let sessionItem forDelete (summary: SessionSummary) =
            let name =
                let extra =
                    if isDebug then
                        let (SessionId guid) = summary.SessionId
                        $" [{guid}]"
                    else
                        ""

                $"""{summary.Name} ({summary.PlaylistCount} {plural "playlist" summary.PlaylistCount}){extra}"""

            let msg = if forDelete then OnDeleteSession else OnOpenSession

            MenuItem.create
                [ MenuItem.header name
                  MenuItem.fontSize 12.
                  if forDelete then
                      MenuItem.foreground COLOUR_DELETE
                  MenuItem.isEnabled (summary.SessionId <> state.Session.Id)
                  MenuItem.onClick ((fun _ -> dispatch (msg summary.SessionId)), OnChangeOf summary.SessionId) ]
            :> IView

        let sortedSummaries =
            state.SessionSummaries |> List.sortBy (fun summary -> summary.Name)

        let enabledCount =
            sortedSummaries
            |> List.filter (fun summary -> summary.SessionId <> state.Session.Id)
            |> List.length

        let openSessionItems = sortedSummaries |> List.map (sessionItem false)

        let deleteSessionItems = sortedSummaries |> List.map (sessionItem true)

        let settingsMenu =
            MenuItem.create
                [ MenuItem.header "Settings"
                  MenuItem.fontSize 12.
                  MenuItem.viewItems
                      [ MenuItem.create
                            [ MenuItem.header (
                                  if state.AutoPlaySession then
                                      "Disable auto-play"
                                  else
                                      "Enable auto-play"
                              )
                              MenuItem.fontSize 12.
                              MenuItem.onClick (fun _ -> dispatch OnToggleAutoPlaySession) ] ] ]

        MenuItem.create
            [ MenuItem.header "Session"
              MenuItem.fontSize 12.
              MenuItem.viewItems
                  [ MenuItem.create
                        [ MenuItem.header "New"
                          MenuItem.fontSize 12.
                          MenuItem.isEnabled canChangeOrDeleteSession
                          MenuItem.onClick (fun _ -> dispatch OnNewSession) ]
                    MenuItem.create
                        [ MenuItem.header "Open"
                          MenuItem.fontSize 12.
                          MenuItem.isEnabled (canChangeOrDeleteSession && enabledCount > 0)
                          MenuItem.viewItems openSessionItems ]
                    MenuItem.create [ MenuItem.header separator ]
                    MenuItem.create
                        [ MenuItem.header "Delete"
                          MenuItem.fontSize 12.
                          MenuItem.foreground COLOUR_DELETE
                          MenuItem.isEnabled (canChangeOrDeleteSession && enabledCount > 0)
                          MenuItem.viewItems deleteSessionItems ]
                    MenuItem.create [ MenuItem.header separator ]
                    settingsMenu
                    MenuItem.create [ MenuItem.header separator ]
                    MenuItem.create
                        [ MenuItem.header "Exit"
                          MenuItem.fontSize 12.
                          MenuItem.onClick (fun _ -> dispatch OnExit) ] ] ]

    let playlistMenu =
        let playlistItem forDelete (summary: PlaylistSummary) =
            let name =
                let extra =
                    if isDebug then
                        let (Playlists.Model.PlaylistId guid) = summary.PlaylistId
                        $" [{guid}]"
                    else
                        ""

                $"""{summary.Name} ({summary.TrackCount} {plural "track" summary.TrackCount}){extra}"""

            let enabled =
                state.PlaylistsState.Playlists
                |> List.exists (fun playlist -> playlist.Id = summary.PlaylistId)
                |> not

            let msg = if forDelete then OnDeletePlaylist else OnOpenPlaylist

            MenuItem.create
                [ MenuItem.header name
                  MenuItem.fontSize 12.
                  if forDelete then
                      MenuItem.foreground COLOUR_DELETE
                  MenuItem.isEnabled enabled
                  MenuItem.onClick ((fun _ -> dispatch (msg summary.PlaylistId)), OnChangeOf summary.PlaylistId) ]
            :> IView

        let sortedSummaries =
            state.PlaylistSummaries |> List.sortBy (fun summary -> summary.Name)

        let enabledCount =
            sortedSummaries
            |> List.filter (fun summary ->
                state.PlaylistsState.Playlists
                |> List.exists (fun playlist -> playlist.Id = summary.PlaylistId)
                |> not)
            |> List.length

        let openPlaylistItems = sortedSummaries |> List.map (playlistItem false)

        let deletePlaylistItems = sortedSummaries |> List.map (playlistItem true)

        let addEnabled = state.PlaylistsState.SelectedPlaylistId |> Option.isSome

        MenuItem.create
            [ MenuItem.header "Playlist"
              MenuItem.fontSize 12.
              MenuItem.viewItems
                  [ MenuItem.create
                        [ MenuItem.header "New"
                          MenuItem.fontSize 12.
                          MenuItem.onClick (fun _ -> dispatch OnNewPlaylist) ]
                    MenuItem.create
                        [ MenuItem.header "Open"
                          MenuItem.fontSize 12.
                          MenuItem.isEnabled (enabledCount > 0)
                          MenuItem.viewItems openPlaylistItems ]
                    MenuItem.create [ MenuItem.header separator ]
                    MenuItem.create
                        [ MenuItem.header ADD_FILES
                          MenuItem.fontSize 12.
                          MenuItem.isEnabled addEnabled
                          MenuItem.onClick (fun _ -> dispatch OnAddFiles) ]
                    MenuItem.create
                        [ MenuItem.header ADD_FOLDER
                          MenuItem.fontSize 12.
                          MenuItem.isEnabled addEnabled
                          MenuItem.onClick (fun _ -> dispatch OnAddFolder) ]
                    MenuItem.create [ MenuItem.header separator ]
                    MenuItem.create
                        [ MenuItem.header "Delete"
                          MenuItem.fontSize 12.
                          MenuItem.foreground COLOUR_DELETE
                          MenuItem.isEnabled (enabledCount > 0)
                          MenuItem.viewItems deletePlaylistItems ] ] ]

    let errorsMenu =
        let hasDebugOnly =
            state.Errors
            |> List.exists (fun (_, _, _, nonDebugMessage) -> nonDebugMessage |> Option.isNone)

        MenuItem.create
            [ MenuItem.header $"Errors ({state.Errors.Length})"
              MenuItem.fontSize 12.
              MenuItem.foreground COLOUR_ERROR
              MenuItem.isEnabled (state.Errors.Length > 0 || state.ShowingDebugOnlyErrors)
              MenuItem.viewItems
                  [ MenuItem.create
                        [ MenuItem.header (
                              if state.ShowingDebugOnlyErrors then
                                  "Hide debug-only"
                              else
                                  "Show debug-only"
                          )
                          MenuItem.fontSize 12.
                          MenuItem.isEnabled hasDebugOnly
                          MenuItem.onClick (fun _ -> dispatch OnToggleShowingDebugOnlyErrors) ]
                    MenuItem.create
                        [ MenuItem.header "Clear all"
                          MenuItem.fontSize 12.
                          MenuItem.foreground COLOUR_REMOVE
                          MenuItem.isEnabled (state.Errors.Length > 0)
                          MenuItem.onClick (fun _ -> dispatch OnClearAllErrors) ] ] ]

    Menu.create
        [ Menu.dock Dock.Top
          Menu.viewItems
              [ sessionMenu
                playlistMenu
                if isDebug then
                    errorsMenu ] ]

let private errorsView showingDebugOnlyErrors (errors: (ErrorId * DateTime * string * string option) list) dispatch =
    let errorTemplate (text, colour: string, errorId: ErrorId option, timestamp: DateTime option) =
        DockPanel.create
            [ DockPanel.verticalAlignment VerticalAlignment.Stretch
              DockPanel.horizontalAlignment HorizontalAlignment.Stretch
              DockPanel.lastChildFill true
              DockPanel.children
                  [ match timestamp with
                    | Some timestamp ->
                        TextBlock.create
                            [ TextBlock.dock Dock.Left
                              TextBlock.verticalAlignment VerticalAlignment.Center
                              TextBlock.textAlignment TextAlignment.Left
                              TextBlock.width 170.
                              TextBlock.fontSize 12.
                              TextBlock.text (timestamp.ToString("yyyy-MM-dd 'at' HH:mm:ss.fff")) ]
                    | None -> ()
                    match errorId with
                    | Some errorId ->
                        Button.create
                            [ Button.dock Dock.Right
                              Button.width SIZE_BUTTON_WITH_ICON
                              Button.height SIZE_BUTTON_WITH_ICON
                              Button.background COLOUR_BACKGROUND
                              Button.cornerRadius 0
                              Button.margin (10, 0, 0, 0)
                              Button.content (Icons.remove true (Some COLOUR_REMOVE) None)
                              Button.tip "Dismiss error"
                              Button.onClick (fun _ -> dispatch (OnDismissError errorId)) ]
                    | None -> ()
                    TextBlock.create
                        [ TextBlock.verticalAlignment VerticalAlignment.Center
                          TextBlock.textAlignment TextAlignment.Left
                          TextBlock.fontSize 12.
                          TextBlock.foreground colour
                          TextBlock.text text ] ] ]

    let errors =
        if isDebug && showingDebugOnlyErrors then
            errors
        else
            errors
            |> List.filter (fun (_, _, _, nonDebugMessage) -> nonDebugMessage |> Option.isSome)

    let errorsForView =
        errors
        |> List.collect (fun (errorId, timestamp, error, nonDebugMessage) ->
            match nonDebugMessage with
            | Some nonDebugMessage ->
                if isDebug then
                    [ nonDebugMessage, COLOUR_INACTIVE, Some errorId, None
                      error, COLOUR_ERROR, None, Some timestamp ]
                else
                    [ nonDebugMessage, COLOUR_INACTIVE, Some errorId, None ]
            | None -> [ error, COLOUR_ERROR, Some errorId, Some timestamp ])

    match errors, isDebug && showingDebugOnlyErrors with
    | _ :: _, _ ->
        ListBox.create
            [ ListBox.dock Dock.Top
              ListBox.maxHeight 154.
              ListBox.background COLOUR_BACKGROUND
              ListBox.dataItems errorsForView
              ListBox.itemTemplate (
                  DataTemplateView<string * string * ErrorId option * DateTime option>.create
                      (fun errorForView -> errorTemplate errorForView)
              ) ]
        :> IView
        |> Some
    | [], true ->
        TextBlock.create
            [ TextBlock.dock Dock.Top
              TextBlock.verticalAlignment VerticalAlignment.Center
              TextBlock.horizontalAlignment HorizontalAlignment.Left
              TextBlock.textAlignment TextAlignment.Left
              TextBlock.padding (10, 0, 0, 0)
              TextBlock.fontSize 12.
              TextBlock.foreground COLOUR_DISABLED_TEXT
              TextBlock.text "- no debug-only errors -" ]
        :> IView
        |> Some
    | [], false -> None

let view state dispatch =
    let session =
        TextBlock.create
            [ TextBlock.dock Dock.Top
              TextBlock.verticalAlignment VerticalAlignment.Center
              TextBlock.horizontalAlignment HorizontalAlignment.Center
              TextBlock.textAlignment TextAlignment.Center
              TextBlock.fontSize 16.
              TextBlock.padding (0, 10, 0, 5)
              TextBlock.foreground COLOUR_INACTIVE
              TextBlock.text state.Session.Name ]

    DockPanel.create
        [ DockPanel.verticalAlignment VerticalAlignment.Stretch
          DockPanel.horizontalAlignment HorizontalAlignment.Stretch
          DockPanel.lastChildFill (state.PlaylistsState.Playlists.Length > 0)
          DockPanel.children
              [ menu state dispatch
                match errorsView state.ShowingDebugOnlyErrors state.Errors dispatch with
                | Some errorsView -> errorsView
                | None -> ()
                session
                yield! Playlists.View.view state.PlaylistsState (PlaylistsMsg >> dispatch) ] ]
