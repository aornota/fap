module Aornota.Fap.App.View

open Aornota.Fap
open Aornota.Fap.App.State
open Aornota.Fap.App.Transition
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

let view state dispatch =
    DockPanel.create
        [ DockPanel.verticalAlignment VerticalAlignment.Stretch
          DockPanel.horizontalAlignment HorizontalAlignment.Stretch
          DockPanel.lastChildFill false
          DockPanel.children
              [ menuBar state dispatch
                Playlists.View.view state.PlaylistsState (PlaylistsMsg >> dispatch)
                Player.View.view state.PlayerState (PlayerMsg >> dispatch) ] ]
