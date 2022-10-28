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
let private NO_ERRORS = "- no errors -"

[<Literal>]
let private TIMESTAMP_FORMAT = "yyyy-MM-dd 'at' HH:mm:ss.fff"

let private menuBar state dispatch =
    let showOrHideErrorsText, showOrHideErrorsColour =
        if state.ShowingErrors then
            "Hide", COLOUR_INACTIVE
        else
            "Show", COLOUR_ACTIVE

    Menu.create
        [ Menu.dock Dock.Top
          Menu.viewItems
              [ MenuItem.create
                    [ MenuItem.header "Debug"
                      MenuItem.fontSize 12.
                      MenuItem.foreground COLOUR_DEBUG
                      MenuItem.viewItems
                          [ MenuItem.create
                                [ MenuItem.header $"Errors ({state.Errors.Length})"
                                  MenuItem.fontSize 12.
                                  MenuItem.foreground COLOUR_ERROR
                                  // If want an "icon"...MenuItem.icon (Image.FromImageAsset "select-files.png")
                                  MenuItem.viewItems
                                      [ MenuItem.create
                                            [ MenuItem.header showOrHideErrorsText
                                              MenuItem.fontSize 12.
                                              MenuItem.foreground showOrHideErrorsColour
                                              MenuItem.onClick (fun _ -> dispatch ToggleShowingErrors) ]
                                        MenuItem.create
                                            [ MenuItem.header "Clear all"
                                              MenuItem.fontSize 12.
                                              MenuItem.foreground COLOUR_REMOVE
                                              MenuItem.isEnabled (state.Errors.Length > 0)
                                              MenuItem.onClick (fun _ -> dispatch ClearAllErrors) ] ] ] ] ] ] ]

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
                          Button.onClick (fun _ -> dispatch (ClearError errorId)) ]
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
    DockPanel.create
        [ DockPanel.verticalAlignment VerticalAlignment.Stretch
          DockPanel.horizontalAlignment HorizontalAlignment.Stretch
          DockPanel.lastChildFill (state.PlaylistsState.Playlists.Length > 0)
          DockPanel.children
              [ if isDebug then
                    menuBar state dispatch
                if isDebug && state.ShowingErrors then
                    errorsView state.Errors dispatch
                Player.View.view state.PlayerState (PlayerMsg >> dispatch)
                Playlists.View.view state.PlaylistsState (PlaylistsMsg >> dispatch) ] ]
