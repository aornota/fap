module Aornota.Fap.Playlists.View

open Aornota.Fap.Domain
open Aornota.Fap.Playlists.Model
open Aornota.Fap.Playlists.Transition
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout

[<Literal>]
let private NO_TRACKS = "- no tracks -"

[<Literal>]
let private SUMMARY = "- summary -"

let private itemView (item: Item) (isPlayingTrackId: TrackId option) dispatch =
    match item with
    | Track track ->
        // TODO-NMB: Show if currently playing - and additional info...
        StackPanel.create
            [ StackPanel.spacing 2.
              StackPanel.onDoubleTapped ((fun _ -> dispatch (PlayTrack track.Id)))
              (* TODO-NMB: Get keyboard stuff working?...
              StackPanel.onKeyUp (fun keyargs ->
                  match keyargs.Key with
                  | Key.Enter -> dispatch (Transition.Msg.PlaySong track)
                  | _ -> ()) *)
              StackPanel.children [ TextBlock.create [ TextBlock.fontSize 12.; TextBlock.text track.Name ] ] ]
    | Summary ->
        // TODO-NMB: Improve this...
        StackPanel.create
            [ StackPanel.spacing 2.
              StackPanel.children [ TextBlock.create [ TextBlock.fontSize 12.; TextBlock.text SUMMARY ] ] ]

let private itemsView (items: NonEmptyList<Item>) selectedTrackId (isPlayingTrackId: TrackId option) dispatch =
    // TODO-NMB: Transform items, e.g. calculate summary/ies...
    let selectedIndex =
        items.List
        |> List.tryFindIndex (function
            | Track track -> track.Id = selectedTrackId
            | Summary _ -> false)

    ListBox.create
        [ ListBox.verticalAlignment VerticalAlignment.Top
          match selectedIndex with
          | Some selectedIndex -> ListBox.selectedIndex selectedIndex
          | None -> ()
          ListBox.dataItems items.List
          ListBox.itemTemplate (DataTemplateView<Item>.create (fun item -> itemView item isPlayingTrackId dispatch)) ]

let private playlistView (playlist: Playlist) dispatch : IView =
    let content =
        match playlist.ItemsState with
        | NoItems ->
            // TODO-NMB: Improve this...
            TextBlock.create [ TextBlock.fontSize 12.; TextBlock.text NO_TRACKS ] :> IView
        | Items (items, selected, isPlaying) -> itemsView items selected isPlaying dispatch
    // TODO-NMB: Show if currently playing (for playlist)...
    TabItem.create
        [ TabItem.header playlist.NameOrDefault
          TabItem.fontSize 14.
          TabItem.content content ]

let view (Playlists playlists) dispatch =
    let tabs =
        playlists.List |> List.map (fun playlist -> playlistView playlist dispatch)

    TabControl.create
        [ TabControl.dock Dock.Top
          TabControl.tabStripPlacement Dock.Top
          TabControl.viewItems tabs ]
