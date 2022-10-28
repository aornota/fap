module Aornota.Fap.Playlists.View

open Aornota.Fap
open Aornota.Fap.Domain
open Aornota.Fap.Literals
open Aornota.Fap.Playlists.Model
open Aornota.Fap.Playlists.Transition
open Aornota.Fap.Utilities
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media

[<Literal>]
let private NO_PLAYLISTS = "- no playlists -"

[<Literal>]
let private NO_TRACKS = "- no tracks -"

// TEMP-NMB...
[<Literal>]
let private SUMMARY = "- summary -"

let private itemsView (items: Item list) (playerStatus: (TrackId * PlayerStatus) option) dispatch =
    let itemTemplate (item: Item, playerStatus: (TrackId * PlayerStatus) option) =
        // TODO-NMB: Conditional handling for Track | Summary...
        let colour, allowPlay =
            match item with
            | Track trackData ->
                match playerStatus with
                | Some (trackId, playerStatus) when trackId = trackData.Id ->
                    match playerStatus with
                    | Active -> COLOUR_ACTIVE, true
                    | Awaiting -> COLOUR_AWAITING, true
                    | Inactive -> COLOUR_INACTIVE, true
                    | Errored -> COLOUR_ERROR, false
                | _ -> COLOUR_DISABLED_TEXT, true
            | Summary -> COLOUR_SUMMARY, false

        let durationText, durationColour =
            match item with
            | Track trackData ->
                let colour =
                    match trackData.Duration with
                    | Some _ -> colour
                    | None -> COLOUR_DISABLED_TEXT

                durationText trackData.Duration, colour
            | Summary -> "", COLOUR_INACTIVE

        let itemText, onDoubleTapped =
            match item with
            | Track trackData ->
                trackData.Name,
                if allowPlay then
                    (fun _ -> dispatch (PlayTrack trackData.Id))
                else
                    ignore
            | Summary -> SUMMARY, ignore

        DockPanel.create
            [ DockPanel.verticalAlignment VerticalAlignment.Stretch
              DockPanel.horizontalAlignment HorizontalAlignment.Stretch
              DockPanel.lastChildFill true
              DockPanel.children
                  [
                    (* Button.create
                        [ Button.dock Dock.Right
                          Button.width SIZE_BUTTON_WITH_ICON
                          Button.height SIZE_BUTTON_WITH_ICON
                          Button.background COLOUR_BACKGROUND
                          Button.cornerRadius 0
                          Button.margin (10, 0, 0, 0)
                          Button.content (Icons.remove true (Some COLOUR_INACTIVE) None)
                          Button.tip "Remove error"
                          Button.onClick (fun _ -> dispatch (RemoveTrack errorId)) ] *)
                    TextBlock.create
                        [ TextBlock.dock Dock.Right
                          TextBlock.verticalAlignment VerticalAlignment.Center
                          TextBlock.textAlignment TextAlignment.Right
                          TextBlock.width 40.
                          TextBlock.fontSize 12.
                          TextBlock.foreground durationColour
                          TextBlock.text durationText ]
                    TextBlock.create
                        [ TextBlock.verticalAlignment VerticalAlignment.Center
                          TextBlock.textAlignment TextAlignment.Left
                          TextBlock.fontSize 12.
                          TextBlock.foreground colour
                          TextBlock.text itemText
                          TextBlock.onDoubleTapped onDoubleTapped ] ] ]

    // TODO-NMB: Transform items (e.g. to TrackDataView | SummaryView) - which should remove need for workaround to prevent weird "player status" cache-ing behaviour?...
    ListBox.create
        [ ListBox.dock Dock.Top
          ListBox.background COLOUR_BACKGROUND
          ListBox.dataItems (items |> List.map (fun item -> item, playerStatus))
          ListBox.itemTemplate (
              DataTemplateView<Item * option<TrackId * PlayerStatus>>.create (fun item -> itemTemplate item)
          ) ]

let private playlistTab (playerStatus: (TrackId * PlayerStatus) option) dispatch playlist : IView =
    let colour =
        match playerStatus with
        | Some (trackId, playerStatus) ->
            if tracks playlist |> List.exists (fun track -> track.Id = trackId) then
                match playerStatus with
                | Active -> COLOUR_ACTIVE
                | Awaiting -> COLOUR_AWAITING
                | Inactive -> COLOUR_INACTIVE
                | Errored -> COLOUR_ERROR
            else
                COLOUR_DISABLED_TEXT
        | None -> COLOUR_DISABLED_TEXT

    let content =
        match playlist.Items with
        | _ :: _ -> itemsView playlist.Items playerStatus dispatch :> IView
        | [] ->
            TextBlock.create
                [ TextBlock.verticalAlignment VerticalAlignment.Top
                  TextBlock.horizontalAlignment HorizontalAlignment.Left
                  TextBlock.textAlignment TextAlignment.Left
                  TextBlock.padding (0, 10, 0, 0)
                  TextBlock.fontSize 12.
                  TextBlock.foreground COLOUR_DISABLED_TEXT
                  TextBlock.text NO_TRACKS ]

    TabItem.create
        [ TabItem.header playlist.Name
          TabItem.foreground colour
          TabItem.fontSize 13.
          TabItem.content content ]

let view state dispatch =
    // TODO-NMB: Show "Session" details?...
    match state.Playlists with
    | _ :: _ ->
        TabControl.create
            [ TabControl.dock Dock.Top
              TabControl.tabStripPlacement Dock.Top
              TabControl.viewItems (state.Playlists |> List.map (playlistTab state.PlayerStatus dispatch)) ]
        :> IView
    | [] ->
        TextBlock.create
            [ TextBlock.dock Dock.Top
              TextBlock.verticalAlignment VerticalAlignment.Center
              TextBlock.horizontalAlignment HorizontalAlignment.Left
              TextBlock.textAlignment TextAlignment.Left
              TextBlock.padding (10, 0, 0, 0)
              TextBlock.fontSize 12.
              TextBlock.foreground COLOUR_DISABLED_TEXT
              TextBlock.text NO_PLAYLISTS ]
