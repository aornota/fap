module Aornota.Fap.Playlists.View

open Aornota.Fap.Domain
open Aornota.Fap.Playlists.State
open Aornota.Fap.Playlists.Transition
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let private itemView (item: Item) (isPlayingTrackId: TrackId option) dispatch =
    match item with
    | Track track ->
        // TODO-NMB: Show if currently playing - and additional info...
        StackPanel.create
            [ StackPanel.spacing 4.0
              StackPanel.onDoubleTapped ((fun _ -> dispatch (PlayTrack track.Id)))
              (* TODO-NMB: Get keyboard stuff working?...
              StackPanel.onKeyUp (fun keyargs ->
                  match keyargs.Key with
                  | Key.Enter -> dispatch (Transition.Msg.PlaySong track)
                  | _ -> ()) *)
              StackPanel.children [ TextBlock.create [ TextBlock.fontSize 12; TextBlock.text track.Name ] ] ]
    | Summary ->
        // TODO-NMB: Improve this...
        StackPanel.create
            [ StackPanel.spacing 4.0
              StackPanel.children [ TextBlock.create [ TextBlock.fontSize 12; TextBlock.text "<summary>" ] ] ]

let private itemsView (items: NonEmptyList<Item>) selectedTrackId (isPlayingTrackId: TrackId option) dispatch =
    // TODO-NMB: Transform items, e.g. calculate summary/ies...
    let selectedIndex =
        items.List
        |> List.tryFindIndex (function
            | Track track -> track.Id = selectedTrackId
            | Summary _ -> false)

    ListBox.create
        [ ListBox.dataItems items.List
          // TODO-NMB: Improve this (e.g. capture "Window resized" and calculate accordingly?)...
          ListBox.maxHeight 430.0
          match selectedIndex with
          | Some selectedIndex -> ListBox.selectedIndex selectedIndex
          | None -> ()
          ListBox.itemTemplate (DataTemplateView<Item>.create (fun item -> itemView item isPlayingTrackId dispatch)) ]

let private playlistView (playlist: State.Playlist) dispatch : IView =
    let content =
        match playlist.ItemsState with
        | NoItems ->
            // TODO-NMB: Improve this...
            TextBlock.create [ TextBlock.fontSize 12; TextBlock.text "<no tracks>" ] :> IView
        | Items (items, selected, isPlaying) -> itemsView items selected isPlaying dispatch
    // TODO-NMB: Show if currently playing (for playlist)...
    TabItem.create
        [ TabItem.header playlist.NameOrDefault
          TabItem.fontSize 14
          TabItem.content content ]

let private playlistsView (playlists: NonEmptyList<Playlist>) dispatch =
    let tabs =
        playlists.List |> List.map (fun playlist -> playlistView playlist dispatch)

    TabControl.create [ TabControl.tabStripPlacement Dock.Top; TabControl.viewItems tabs ]

let view (Playlists playlists) dispatch =
    StackPanel.create
        [ StackPanel.dock Dock.Top
          StackPanel.children [ playlistsView playlists dispatch ] ]
