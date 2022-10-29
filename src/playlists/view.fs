module Aornota.Fap.Playlists.View

open Aornota.Fap
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

[<Literal>]
let private NO_SELECTED_TRACK = "- no track selected -"

let private colour playerState =
    match playerState with
    | NoMedia
    | Stopped _
    | Ended -> COLOUR_INACTIVE
    | AwaitingPlay _
    | Paused _ -> COLOUR_AWAITING
    | Playing _ -> COLOUR_ACTIVE
    | PlaybackErrored -> COLOUR_ERROR

let private itemsView (items: Item list) (trackState: TrackState option) dispatch =
    let itemTemplate (item, trackState) =
        // TODO-NMB: Conditional handling for Track | Summary...
        let colour, allowPlay =
            match item with
            | Track trackData ->
                match trackState with
                | Some trackState when trackData.Id = trackState.Track.Id ->
                    colour trackState.PlayerState, trackState.PlayerState <> PlaybackErrored
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
                    (fun _ -> dispatch (OnPlayTrack trackData.Id))
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
          ListBox.dataItems (items |> List.map (fun item -> item, trackState))
          ListBox.itemTemplate (DataTemplateView<Item * TrackState option>.create (fun item -> itemTemplate item)) ]

let private playlistTab selectedPlaylistId trackState dispatch playlist : IView =
    let colour =
        match trackState with
        | Some trackState ->
            if tracks playlist |> List.exists (fun track -> track.Id = trackState.Track.Id) then
                colour trackState.PlayerState
            else
                COLOUR_DISABLED_TEXT
        | None -> COLOUR_DISABLED_TEXT

    let content =
        match playlist.Items with
        | _ :: _ -> itemsView playlist.Items trackState dispatch :> IView
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
          TabItem.isSelected (Some playlist.Id = selectedPlaylistId)
          TabItem.content content
          // TODO-NMB: Seems to trigger for first tab even if not selected?...
          TabItem.onIsSelectedChanged (fun selected ->
              if selected then
                  dispatch (OnSelectPlaylist playlist.Id)) ]

let private playlistsView state dispatch =
    match state.Playlists with
    | _ :: _ ->
        TabControl.create
            [ TabControl.dock Dock.Top
              TabControl.tabStripPlacement Dock.Top
              TabControl.viewItems (
                  state.Playlists
                  |> List.map (playlistTab state.SelectedPlaylistId state.TrackState dispatch)
              ) ]
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

let private progressBar trackState (colour: string) dispatch =
    let enabled, positionValue, duration =
        match trackState with
        | Some trackState ->
            let enabled, position =
                match trackState.PlayerState with
                | Playing (position, _)
                | Paused position
                | Stopped position -> true, position
                | _ -> false, START_POSITION

            enabled, position, trackState.Track.Duration
        | None -> false, START_POSITION, None

    let position =
        TextBlock.create
            [ TextBlock.verticalAlignment VerticalAlignment.Center
              TextBlock.textAlignment TextAlignment.Right
              TextBlock.width 40.
              TextBlock.fontSize 12.
              TextBlock.foreground colour
              TextBlock.text (positionText positionValue duration) ]

    let slider =
        Slider.create
            [ Slider.horizontalAlignment HorizontalAlignment.Center
              Slider.width 500.
              Slider.minimum 0.
              Slider.maximum 100.
              Slider.foreground colour
              Slider.isEnabled enabled
              Slider.value (positionValue * 100f |> double)
              Slider.tip "Seek within track"
              Slider.onValueChanged (fun value -> dispatch (OnSeek(value / 100. |> float32))) ]

    let durationColour =
        match duration with
        | Some _ -> colour
        | None -> COLOUR_DISABLED_TEXT

    let duration =
        TextBlock.create
            [ TextBlock.verticalAlignment VerticalAlignment.Center
              TextBlock.textAlignment TextAlignment.Left
              TextBlock.width 40.
              TextBlock.fontSize 12.
              TextBlock.foreground durationColour
              TextBlock.text (durationText duration) ]

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Center
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Horizontal
          StackPanel.spacing 10.
          StackPanel.children [ position; slider; duration ] ]

let private trackDetails trackState (colour: string) =
    let details =
        match trackState with
        | Some trackState -> trackState.Track.Name
        | None -> NO_SELECTED_TRACK

    TextBlock.create
        [ TextBlock.horizontalAlignment HorizontalAlignment.Center
          TextBlock.fontSize 12
          TextBlock.foreground colour
          TextBlock.text details ]

let private media state dispatch =
    let button
        (fIcon: bool -> string option -> string option -> IView<Canvas>)
        enabled
        enabledColourOverride
        disabledColourOverride
        leftMargin
        (tip: string)
        onClick
        =
        Button.create
            [ Button.width SIZE_BUTTON_WITH_ICON
              Button.height SIZE_BUTTON_WITH_ICON
              Button.background COLOUR_BACKGROUND
              Button.margin (leftMargin, 0, 0, 0)
              Button.cornerRadius 0
              Button.content (fIcon enabled enabledColourOverride disabledColourOverride)
              if enabled then
                  Button.tip tip
              Button.onClick (if enabled then onClick else ignore) ]

    let isPlayingOrAwaitingPlay, allowPrevious, allowNext, allowPlay, playDisabledColourOverride, allowPause, allowStop =
        match state.TrackState with
        | Some trackState ->
            match trackState.PlayerState with
            | NoMedia -> false, trackState.HasPrevious, trackState.HasNext, true, None, false, false
            | AwaitingPlay _ ->
                true, trackState.HasPrevious, trackState.HasNext, false, Some COLOUR_AWAITING, false, false
            | PlaybackErrored ->
                false, trackState.HasPrevious, trackState.HasNext, false, Some COLOUR_ERROR, false, false
            | Playing _ -> true, trackState.HasPrevious, trackState.HasNext, false, None, true, true
            | Paused _ -> false, trackState.HasPrevious, trackState.HasNext, true, None, false, true
            | Stopped _
            | Ended -> false, trackState.HasPrevious, trackState.HasNext, true, None, false, false
        | None -> false, false, false, false, None, false, false

    let previousAndNextEnabledColourOverride =
        if isPlayingOrAwaitingPlay then
            COLOUR_ACTIVE
        else
            COLOUR_INACTIVE

    let muteOrUnmuteIcon, muteOrUnmuteTip =
        if state.Muted then
            Icons.muted, "Unmute"
        else
            let icon =
                match state.Volume with
                | volume when volume < 34 -> Icons.unmutedLow
                | volume when volume < 67 -> Icons.unmutedMedium
                | _ -> Icons.unmutedHigh

            icon, "Mute"

    StackPanel.create
        [ StackPanel.verticalAlignment VerticalAlignment.Center
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Horizontal
          StackPanel.margin (0, 15, 0, 15)
          StackPanel.children
              [ button
                    Icons.previous
                    allowPrevious
                    (Some previousAndNextEnabledColourOverride)
                    None
                    0
                    "Previous track"
                    (fun _ -> dispatch OnPrevious)

                if allowPause then
                    button Icons.pause true (Some COLOUR_AWAITING) None 6 "Pause track" (fun _ -> dispatch OnPause)
                else
                    button Icons.play allowPlay None playDisabledColourOverride 6 "Play track" (fun _ -> dispatch OnPlay)

                button Icons.stop allowStop (Some COLOUR_INACTIVE) None 0 "Stop track" (fun _ -> dispatch OnStop)
                button Icons.next allowNext (Some previousAndNextEnabledColourOverride) None 6 "Next track" (fun _ ->
                    dispatch OnNext)
                button
                    muteOrUnmuteIcon
                    (state.Volume <> 0)
                    (Some COLOUR_VOLUME)
                    (Some COLOUR_VOLUME)
                    20
                    muteOrUnmuteTip
                    (fun _ -> dispatch OnToggleMuted)
                Slider.create
                    [ Slider.verticalAlignment VerticalAlignment.Center
                      Slider.horizontalAlignment HorizontalAlignment.Center
                      Slider.width 100.
                      Slider.minimum 0.
                      Slider.maximum 100.
                      Slider.margin (8, 0, 0, 0)
                      Slider.padding (0, 0, 0, 6)
                      Slider.foreground COLOUR_VOLUME
                      Slider.value state.Volume
                      Slider.tip $"Volume: {state.Volume}%%"
                      Slider.onValueChanged (fun value -> dispatch (OnVolume(value |> int))) ] ] ]

let private playerView state dispatch =
    let colour =
        match state.TrackState with
        | Some trackState -> colour trackState.PlayerState
        | None -> COLOUR_DISABLED_TEXT

    StackPanel.create
        [ StackPanel.dock Dock.Bottom
          StackPanel.verticalAlignment VerticalAlignment.Center
          StackPanel.horizontalAlignment HorizontalAlignment.Center
          StackPanel.orientation Orientation.Vertical
          StackPanel.children
              [ progressBar state.TrackState colour dispatch
                trackDetails state.TrackState colour
                media state dispatch ] ]

let view state dispatch =
    let trackCount =
        state.Playlists |> List.collect (fun playlist -> tracks playlist) |> List.length

    [ playlistsView state dispatch
      if trackCount > 0 then
          playerView state dispatch ]
