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

type private TrackForView =
    { TrackData: TrackData
      Colour: string
      CanMove: Direction list
      CanAddSummary: RelativePosition option }

type private TotalType =
    | Complete
    | Partial

type private SummaryForView =
    { Id: SummaryId option
      TrackCount: int
      TotalDuration: TotalType * int64<millisecond> }

type private ItemForView =
    | TrackForView of TrackForView
    | SummaryForView of SummaryForView

[<Literal>]
let private NO_PLAYLISTS = "- no playlists -"

[<Literal>]
let private NO_TRACKS = "- no tracks -"

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

let private button
    (fIcon: bool -> string option -> string option -> IView<Canvas>)
    dock
    enabled
    enabledColourOverride
    disabledColourOverride
    leftMargin
    (tip: string)
    onClick
    =
    Button.create
        [ Button.dock dock
          Button.width SIZE_BUTTON_WITH_ICON
          Button.height SIZE_BUTTON_WITH_ICON
          Button.background COLOUR_BACKGROUND
          Button.margin (leftMargin, 0, 0, 0)
          Button.cornerRadius 0
          Button.content (fIcon enabled enabledColourOverride disabledColourOverride)
          if enabled then
              Button.tip tip
          Button.onClick (if enabled then onClick else ignore) ]

let private transformItems
    (items: Item list)
    isFirstPlaylist
    isLastPlaylist
    (trackState: TrackState option)
    : ItemForView list =
    let isSummary item =
        match item with
        | Some item ->
            match item with
            | Summary _ -> true
            | Track _ -> false
        | None -> false

    match items with
    | _ :: _ ->
        let items =
            match items with
            | Summary _ :: t -> t
            | _ -> items

        let firstItem, lastItem = items |> List.head, items |> List.rev |> List.head

        let items =
            if not (isSummary (Some lastItem)) then
                items @ [ Summary None ]
            else
                Summary None :: (items |> List.rev |> List.tail)

        let (itemsWithPrevious, _) =
            items
            |> List.fold
                (fun (itemsWithPrevious, previous) item -> (item, previous) :: itemsWithPrevious, Some item)
                ([], None)

        let (itemsWithPreviousAndNext, _) =
            itemsWithPrevious
            |> List.fold
                (fun (itemsWithPreviousAndNext, next) (item, previous) ->
                    (item, (previous, next)) :: itemsWithPreviousAndNext, Some item)
                ([], None)

        let itemsForView, _ =
            itemsWithPreviousAndNext
            |> List.fold
                (fun (itemsForView, durations) (item, (previous, next)) ->
                    match item with
                    | Track trackData ->
                        let colour =
                            match trackState with
                            | Some trackState when trackData.Id = trackState.Track.Id -> colour trackState.PlayerState
                            | _ -> COLOUR_DISABLED_TEXT

                        let isFirstItem = isTrackId trackData.Id firstItem
                        let isLastItem = isTrackId trackData.Id lastItem

                        let canMove = if not isFirstItem then [ Up ] else []
                        let canMove = if not isLastItem then Down :: canMove else canMove
                        let canMove = if not isFirstPlaylist then Left :: canMove else canMove
                        let canMove = if not isLastPlaylist then Right :: canMove else canMove

                        let canAddSummary =
                            if isFirstItem && not isLastItem && not (isSummary next) then
                                Some Below
                            else if isLastItem && not isFirstItem && not (isSummary previous) then
                                Some Above
                            else
                                None

                        let itemForView =
                            { TrackData = trackData
                              Colour = colour
                              CanMove = canMove
                              CanAddSummary = canAddSummary }

                        TrackForView itemForView :: itemsForView, trackData.Duration :: durations
                    | Summary summaryId ->
                        let trackCount = durations.Length
                        let knownDurations = durations |> List.choose id

                        let totalType =
                            if trackCount = knownDurations.Length then
                                Complete
                            else
                                Partial

                        let itemForView =
                            { Id = summaryId
                              TrackCount = trackCount
                              TotalDuration = (totalType, knownDurations |> List.sum) }

                        SummaryForView itemForView :: itemsForView, [])
                ([], [])

        itemsForView |> List.rev
    | [] -> []

let private itemsView items isFirstPlaylist isLastPlaylist trackState dispatch =
    let itemForViewTemplate =
        function
        | TrackForView track ->
            let allowPlay =
                match trackState with
                | Some trackState when track.TrackData.Id = trackState.Track.Id ->
                    trackState.PlayerState <> PlaybackErrored
                | _ -> true

            let durationText = durationText track.TrackData.Duration

            let durationColour =
                match track.TrackData.Duration with
                | Some _ -> track.Colour
                | None -> COLOUR_DISABLED_TEXT

            let moveUpOrAddBelow =
                match track.CanAddSummary with
                | Some Below ->
                    button Icons.addBelow Dock.Left true (Some COLOUR_SUMMARY) None 0 "Add summary below" (fun _ ->
                        dispatch (OnAddSummary(track.TrackData.Id, Below)))
                | _ ->
                    button
                        Icons.up
                        Dock.Left
                        (track.CanMove |> List.contains Up)
                        (Some track.Colour)
                        None
                        0
                        "Move track up"
                        (fun _ -> dispatch (OnMoveTrack(track.TrackData.Id, Up)))

            let moveDownOrAddAbove =
                match track.CanAddSummary with
                | Some Above ->
                    button Icons.addAbove Dock.Left true (Some COLOUR_SUMMARY) None 0 "Add summary above" (fun _ ->
                        dispatch (OnAddSummary(track.TrackData.Id, Above)))
                | _ ->
                    button
                        Icons.down
                        Dock.Left
                        (track.CanMove |> List.contains Down)
                        (Some track.Colour)
                        None
                        0
                        "Move track down"
                        (fun _ -> dispatch (OnMoveTrack(track.TrackData.Id, Down)))

            DockPanel.create
                [ DockPanel.verticalAlignment VerticalAlignment.Stretch
                  DockPanel.horizontalAlignment HorizontalAlignment.Stretch
                  DockPanel.lastChildFill true
                  DockPanel.children
                      [ moveUpOrAddBelow
                        moveDownOrAddAbove
                        button Icons.remove Dock.Right true (Some COLOUR_REMOVE) None 6 "Remove track" (fun _ ->
                            dispatch (OnRemoveTrack track.TrackData.Id))
                        button
                            Icons.right
                            Dock.Right
                            (track.CanMove |> List.contains Right)
                            (Some track.Colour)
                            None
                            0
                            "Move track right"
                            (fun _ -> dispatch (OnMoveTrack(track.TrackData.Id, Right)))
                        button
                            Icons.left
                            Dock.Right
                            (track.CanMove |> List.contains Left)
                            (Some track.Colour)
                            None
                            12
                            "Move track left"
                            (fun _ -> dispatch (OnMoveTrack(track.TrackData.Id, Left)))
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
                              TextBlock.margin (12, 0, 0, 0)
                              TextBlock.foreground track.Colour
                              TextBlock.text track.TrackData.Name
                              if allowPlay then
                                  TextBlock.onDoubleTapped (fun _ -> dispatch (OnPlayTrack track.TrackData.Id)) ] ] ]
        | SummaryForView summary ->
            let (totalType, duration) = summary.TotalDuration
            let durationText = durationText (Some duration)

            let durationText =
                match totalType with
                | Complete -> durationText
                | Partial -> $"({durationText})+"

            let summaryText =
                $"""{summary.TrackCount} {plural "track" summary.TrackCount} | {durationText}"""

            DockPanel.create
                [ DockPanel.verticalAlignment VerticalAlignment.Stretch
                  DockPanel.horizontalAlignment HorizontalAlignment.Stretch
                  DockPanel.lastChildFill true
                  DockPanel.children
                      [ match summary.Id with
                        | Some summaryId ->
                            button Icons.remove Dock.Right true (Some COLOUR_REMOVE) None 0 "Remove summary" (fun _ ->
                                dispatch (OnRemoveSummary summaryId))
                        | None -> ()
                        TextBlock.create
                            [ TextBlock.verticalAlignment VerticalAlignment.Center
                              TextBlock.textAlignment TextAlignment.Left
                              TextBlock.fontSize 12.
                              TextBlock.fontWeight FontWeight.DemiBold
                              TextBlock.margin (72, 0, 0, 0)
                              TextBlock.foreground COLOUR_SUMMARY
                              TextBlock.text summaryText ] ] ]

    ListBox.create
        [ ListBox.dock Dock.Top
          ListBox.background COLOUR_BACKGROUND
          ListBox.dataItems (transformItems items isFirstPlaylist isLastPlaylist trackState)
          ListBox.itemTemplate (
              DataTemplateView<ItemForView>.create (fun itemForView -> itemForViewTemplate itemForView)
          ) ]

let private playlistTab firstAndLastPlaylistIds selectedPlaylistId trackState dispatch playlist : IView =
    let colour =
        match trackState with
        | Some trackState ->
            if tracks playlist |> List.exists (fun track -> track.Id = trackState.Track.Id) then
                colour trackState.PlayerState
            else
                COLOUR_DISABLED_TEXT
        | None -> COLOUR_DISABLED_TEXT

    let isFirstPlaylist = playlist.Id = fst firstAndLastPlaylistIds
    let isLastPlaylist = playlist.Id = snd firstAndLastPlaylistIds

    let controls =
        DockPanel.create
            [ DockPanel.dock Dock.Top
              DockPanel.verticalAlignment VerticalAlignment.Stretch
              DockPanel.horizontalAlignment HorizontalAlignment.Stretch
              DockPanel.lastChildFill false
              DockPanel.children
                  [ button
                        Icons.left
                        Dock.Left
                        (not isFirstPlaylist)
                        (Some COLOUR_INACTIVE)
                        None
                        0
                        "Move playlist left"
                        (fun _ -> dispatch (OnMovePlaylist(playlist.Id, Left)))
                    button
                        Icons.right
                        Dock.Left
                        (not isLastPlaylist)
                        (Some COLOUR_INACTIVE)
                        None
                        0
                        "Move playlist right"
                        (fun _ -> dispatch (OnMovePlaylist(playlist.Id, Right)))
                    button Icons.remove Dock.Right true (Some COLOUR_REMOVE) None 0 "Remove playlist" (fun _ ->
                        dispatch (OnRemovePlaylist playlist.Id)) ] ]

    let content =
        match playlist.Items with
        | _ :: _ -> itemsView playlist.Items isFirstPlaylist isLastPlaylist trackState dispatch :> IView
        | [] ->
            TextBlock.create
                [ TextBlock.verticalAlignment VerticalAlignment.Top
                  TextBlock.horizontalAlignment HorizontalAlignment.Left
                  TextBlock.textAlignment TextAlignment.Left
                  TextBlock.padding (0, 10, 0, 0)
                  TextBlock.fontSize 12.
                  TextBlock.foreground COLOUR_DISABLED_TEXT
                  TextBlock.text NO_TRACKS ]

    let controlsAndContent =
        DockPanel.create
            [ DockPanel.verticalAlignment VerticalAlignment.Stretch
              DockPanel.horizontalAlignment HorizontalAlignment.Stretch
              DockPanel.lastChildFill true
              DockPanel.children [ controls; content ] ]

    TabItem.create
        [ TabItem.header playlist.Name
          TabItem.foreground colour
          TabItem.fontSize 13.
          TabItem.isSelected (Some playlist.Id = selectedPlaylistId)
          TabItem.content controlsAndContent
          TabItem.onTapped (fun _ -> dispatch (OnSelectPlaylist playlist.Id)) ]

let private playlistsView (playlists: Playlist list) selectedPlaylistId trackState dispatch =
    match playlists with
    | _ :: _ ->
        let firstAndLastPlaylistIds =
            (playlists |> List.head).Id, (playlists |> List.rev |> List.head).Id

        TabControl.create
            [ TabControl.dock Dock.Top
              TabControl.tabStripPlacement Dock.Top
              TabControl.viewItems (
                  playlists
                  |> List.map (playlistTab firstAndLastPlaylistIds selectedPlaylistId trackState dispatch)
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
    :> IView

let view state dispatch =
    let trackCount =
        state.Playlists |> List.collect (fun playlist -> tracks playlist) |> List.length

    [ if trackCount > 0 then
          playerView state dispatch
      playlistsView state.Playlists state.SelectedPlaylistId state.TrackState dispatch ]
