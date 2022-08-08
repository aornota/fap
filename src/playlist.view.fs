[<RequireQualifiedAccess>]
module Aornota.Fap.Playlist.View

open Aornota.Fap.Domain
open Aornota.Fap.Playlist
open Avalonia.Controls
open Avalonia.Input
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types

let private songTemplate (song: SongRecord) dispatch =
    StackPanel.create
        [ StackPanel.spacing 8.0
          StackPanel.onDoubleTapped ((fun _ -> dispatch (Transition.Msg.PlaySong song)))
          StackPanel.onKeyUp (fun keyargs ->
              match keyargs.Key with
              | Key.Enter -> dispatch (Transition.Msg.PlaySong song)
              // TODO-NMB: Add other shortcuts to rearrange songs (&c.)?...
              | _ -> ())
          StackPanel.children [ TextBlock.create [ TextBlock.text song.name ] ] ]

let private songRecordList (selectedIndex: int) (songs: SongRecord list) dispatch =
    ListBox.create
        [ ListBox.dataItems songs
          ListBox.maxHeight 420.0
          ListBox.selectedIndex selectedIndex
          ListBox.itemTemplate (DataTemplateView<SongRecord>.create (fun item -> songTemplate item dispatch)) ]

let private emptySongList = StackPanel.create [ StackPanel.spacing 8.0 ]

let private songList (state: State.State) dispatch =
    match state.SongList with
    | Some songs ->
        match songs with
        | [] -> emptySongList :> IView
        | _ -> songRecordList state.CurrentIndex songs dispatch :> IView
    | None -> emptySongList :> IView

let view state dispatch =
    StackPanel.create [ StackPanel.dock Dock.Top; StackPanel.children [ songList state dispatch ] ]
