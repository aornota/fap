module Aornota.Fap.Utilities

open Aornota.Fap.Literals
open System

[<Measure>]
type millisecond

[<Literal>]
let SECOND = 1000L<millisecond>

[<Literal>]
let SECONDS_PER_MINUTE = 60L

[<Literal>]
let MINUTES_PER_HOUR = 60L

let private minute = SECOND * SECONDS_PER_MINUTE
let private hour = minute * MINUTES_PER_HOUR

let private formatTime roundUp (value: int64<millisecond>) =
    let hours = if value > hour then Some(value / hour) else None

    let value =
        match hours with
        | Some hours -> value - (hours * hour)
        | None -> value

    let minutes = if value > minute then value / minute else 0
    let value = value - (minutes * minute)
    let seconds = if value > SECOND then value / SECOND else 0
    let milliseconds = value - (seconds * SECOND)

    let hours, minutes, seconds =
        if roundUp then
            let seconds =
                if milliseconds > (SECOND / 2L) then
                    seconds + 1L
                else
                    seconds

            let minutes, seconds =
                if seconds = SECONDS_PER_MINUTE then
                    minutes + 1L, 0L
                else
                    minutes, seconds

            let hours, minutes =
                if minutes = MINUTES_PER_HOUR then
                    let hours =
                        match hours with
                        | Some hours -> Some(hours + 1L)
                        | None -> Some(1L)

                    hours, 0L
                else
                    hours, minutes

            hours, minutes, seconds
        else
            hours, minutes, seconds

    let minutesPrefix = if minutes < 10 then "0" else ""
    let secondsPrefix = if seconds < 10 then "0" else ""

    let minutesAndSeconds = $"{minutesPrefix}{minutes}:{secondsPrefix}{seconds}"

    match hours with
    | Some hours -> $"{hours}:{minutesAndSeconds}"
    | None -> minutesAndSeconds

let durationText =
    function
    | Some duration -> formatTime true duration
    | None -> "N/A"

let positionText (positionValue: float32) (duration: int64<millisecond> option) =
    match duration with
    | Some duration ->
        let position = Math.Round((positionValue |> double) * (duration |> double)) |> int64
        formatTime false (position * 1L<millisecond>)
    | None -> "0:00"

let playerVolume (volume: int) =
    match Math.Max(Math.Min(volume, 100), 0) with
    | 0 -> 0
    | volume -> Math.Log10(volume) * 50. |> int

let plural text count = if count = 1 then text else $"{text}s"

let fileExtensions = [ FILE_EXTENSION_FLAC; FILE_EXTENSION_MP3; FILE_EXTENSION_WAV ]

let isDebug =
#if DEBUG
    true
#else
    false
#endif
