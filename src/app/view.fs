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

[<Literal>]
let private MENU_ITEM_SEPARATOR = "-"

[<Literal>]
let private NO_ERRORS = "- no errors -"

[<Literal>]
let private TIMESTAMP_FORMAT = "yyyy-MM-dd 'at' HH:mm:ss.fff"

let private menu state dispatch =
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
                    MenuItem.create [ MenuItem.header MENU_ITEM_SEPARATOR ]
                    MenuItem.create
                        [ MenuItem.header "Delete"
                          MenuItem.fontSize 12.
                          MenuItem.foreground COLOUR_DELETE
                          MenuItem.isEnabled (canChangeOrDeleteSession && enabledCount > 0)
                          MenuItem.viewItems deleteSessionItems ]
                    MenuItem.create [ MenuItem.header MENU_ITEM_SEPARATOR ]
                    settingsMenu
                    MenuItem.create [ MenuItem.header MENU_ITEM_SEPARATOR ]
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
                    MenuItem.create [ MenuItem.header MENU_ITEM_SEPARATOR ]
                    MenuItem.create [ MenuItem.header "Add files"; MenuItem.fontSize 12.; MenuItem.onClick ignore ]
                    MenuItem.create [ MenuItem.header "Add folder"; MenuItem.fontSize 12.; MenuItem.onClick ignore ]
                    MenuItem.create [ MenuItem.header MENU_ITEM_SEPARATOR ]
                    MenuItem.create
                        [ MenuItem.header "Delete"
                          MenuItem.fontSize 12.
                          MenuItem.foreground COLOUR_DELETE
                          MenuItem.isEnabled (enabledCount > 0)
                          MenuItem.viewItems deletePlaylistItems ] ] ]

    let errorsMenu =
        MenuItem.create
            [ MenuItem.header $"Errors ({state.Errors.Length})"
              MenuItem.fontSize 12.
              MenuItem.foreground COLOUR_ERROR
              MenuItem.isEnabled (state.Errors.Length > 0 || state.ShowingErrors)
              MenuItem.viewItems
                  [ MenuItem.create
                        [ MenuItem.header (if state.ShowingErrors then "Hide" else "Show")
                          MenuItem.fontSize 12.
                          MenuItem.onClick (fun _ -> dispatch OnToggleShowingErrors) ]
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
                if isDebug then
                    playlistMenu
                    errorsMenu ] ]

let private errorsView (errors: (ErrorId * DateTime * string) list) dispatch =
    let errorTemplate (errorId, timestamp: DateTime, message) =
        DockPanel.create
            [ DockPanel.verticalAlignment VerticalAlignment.Stretch
              DockPanel.horizontalAlignment HorizontalAlignment.Stretch
              DockPanel.lastChildFill true
              DockPanel.children
                  [ TextBlock.create
                        [ TextBlock.dock Dock.Left
                          TextBlock.verticalAlignment VerticalAlignment.Center
                          TextBlock.textAlignment TextAlignment.Left
                          TextBlock.width 170.
                          TextBlock.fontSize 12.
                          TextBlock.text (timestamp.ToString(TIMESTAMP_FORMAT)) ]
                    Button.create
                        [ Button.dock Dock.Right
                          Button.width SIZE_BUTTON_WITH_ICON
                          Button.height SIZE_BUTTON_WITH_ICON
                          Button.background COLOUR_BACKGROUND
                          Button.cornerRadius 0
                          Button.margin (10, 0, 0, 0)
                          Button.content (Icons.remove true (Some COLOUR_REMOVE) None)
                          Button.tip "Clear error"
                          Button.onClick (fun _ -> dispatch (OnClearError errorId)) ]
                    TextBlock.create
                        [ TextBlock.verticalAlignment VerticalAlignment.Center
                          TextBlock.textAlignment TextAlignment.Left
                          TextBlock.fontSize 12.
                          TextBlock.foreground COLOUR_ERROR
                          TextBlock.text message ] ] ]

    if errors.Length > 0 then
        ListBox.create
            [ ListBox.dock Dock.Top
              ListBox.maxHeight 154.
              ListBox.background COLOUR_BACKGROUND
              ListBox.dataItems errors
              ListBox.itemTemplate (
                  DataTemplateView<ErrorId * DateTime * string>.create (fun error -> errorTemplate error)
              ) ]
        :> IView
    else
        TextBlock.create
            [ TextBlock.dock Dock.Top
              TextBlock.verticalAlignment VerticalAlignment.Center
              TextBlock.horizontalAlignment HorizontalAlignment.Left
              TextBlock.textAlignment TextAlignment.Left
              TextBlock.padding (10, 0, 0, 0)
              TextBlock.fontSize 12.
              TextBlock.foreground COLOUR_DISABLED_TEXT
              TextBlock.text NO_ERRORS ]

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
                if isDebug && state.ShowingErrors then
                    errorsView state.Errors dispatch
                session
                yield! Playlists.View.view state.PlaylistsState (PlaylistsMsg >> dispatch) ] ]
