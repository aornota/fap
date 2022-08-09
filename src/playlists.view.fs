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
            [ StackPanel.spacing 8.0
              StackPanel.onDoubleTapped ((fun _ -> dispatch (PlayTrack track.Id)))
              (* TODO-NMB: Get keyboard stuff working?...
              StackPanel.onKeyUp (fun keyargs ->
                  match keyargs.Key with
                  | Key.Enter -> dispatch (Transition.Msg.PlaySong track)
                  | _ -> ()) *)
              StackPanel.children [ TextBlock.create [ TextBlock.text track.Name ] ] ]
    | Summary ->
        // TODO-NMB: Improve this...
        StackPanel.create
            [ StackPanel.spacing 8.0
              StackPanel.children [ TextBlock.create [ TextBlock.text "TODO-NMB: Summary..." ] ] ]

let private itemsView (items: NonEmptyList<Item>) selectedTrackId (isPlayingTrackId: TrackId option) dispatch =
    // TODO-NMB: Transform items, e.g. calculate summary/ies...
    let selectedIndex =
        items.List
        |> List.tryFindIndex (function
            | Track track -> track.Id = selectedTrackId
            | Summary _ -> false)

    ListBox.create
        [ ListBox.dataItems items.List
          // TODO-NMB: Is this necessary?...ListBox.maxHeight 420.0
          match selectedIndex with
          | Some selectedIndex -> ListBox.selectedIndex selectedIndex
          | None -> ()
          ListBox.itemTemplate (DataTemplateView<Item>.create (fun item -> itemView item isPlayingTrackId dispatch)) ]

let private playlistView (playlist: State.Playlist) selectedPlaylistId dispatch : IView =
    let content =
        match playlist.ItemsState with
        | NoItems ->
            // TODO-NMB: Improve this...
            TextBlock.create [ TextBlock.text "No tracks" ] :> IView
        | Items (items, selected, isPlaying) -> itemsView items selected isPlaying dispatch
    // TODO-NMB: Show if currently playing (for playlist)...
    TabItem.create
        [ TabItem.header playlist.Name
          TabItem.isSelected (playlist.Id = selectedPlaylistId)
          TabItem.content content ]

let private playlistsView (playlists: NonEmptyList<Playlist>) selectedPlaylistId dispatch =
    let tabs =
        playlists.List
        |> List.map (fun playlist -> playlistView playlist selectedPlaylistId dispatch)

    TabControl.create [ TabControl.tabStripPlacement Dock.Top; TabControl.viewItems tabs ]

let view (state: State.State) dispatch =
    let children =
        match state with
        | NoPlaylists ->
            // TODO-NMB: Improve this...
            TextBlock.create [ TextBlock.text "No playlists" ] :> IView
        | Playlists (playlists, selected) -> playlistsView playlists selected dispatch

    StackPanel.create [ StackPanel.dock Dock.Top; StackPanel.children [ children ] ]
