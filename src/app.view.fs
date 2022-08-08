[<RequireQualifiedAccess>]
module Aornota.Fap.App.View

open Aornota.Fap
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.FuncUI.DSL

let private menuBar (_: State.State) dispatch =
    Menu.create
        [ Menu.dock Dock.Top
          Menu.viewItems
              [ MenuItem.create
                    [ MenuItem.header "Playlist"
                      MenuItem.viewItems
                          [ MenuItem.create
                                [ MenuItem.header "Select files"
                                  MenuItem.icon (Image.FromString "avares://fap/assets/icons/select-files.png")
                                  MenuItem.onClick (fun _ -> dispatch Transition.Msg.OpenFiles) ]
                            MenuItem.create
                                [ MenuItem.header "Select folder"
                                  MenuItem.icon (Image.FromString "avares://fap/assets/icons/select-folder.png")
                                  MenuItem.onClick (fun _ -> dispatch Transition.Msg.OpenFolder) ] ] ] ] ]

let view (state: State.State) (dispatch: Transition.Msg -> unit) =
    DockPanel.create
        [ DockPanel.verticalAlignment VerticalAlignment.Stretch
          DockPanel.horizontalAlignment HorizontalAlignment.Stretch
          DockPanel.lastChildFill false
          DockPanel.children
              [ menuBar state dispatch
                Playlist.View.view state.PlaylistState (Transition.Msg.PlaylistMsg >> dispatch)
                Player.View.view state.PlayerState (Transition.Msg.PlayerMsg >> dispatch) ] ]
