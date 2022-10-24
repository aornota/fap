module Aornota.Fap.Utilities

open System

[<Measure>]
type millisecond

type TimeFormat =
    | NoRounding
    | RoundUp
    | IncludeMilliseconds

[<Literal>]
let SECOND = 1000L<millisecond>

[<Literal>]
let SECONDS_PER_MINUTE = 60L

[<Literal>]
let MINUTES_PER_HOUR = 60L

let private minute = SECOND * SECONDS_PER_MINUTE
let private hour = minute * MINUTES_PER_HOUR

let private formatTime timeFormat (value: int64<millisecond>) =
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
        if timeFormat = RoundUp then
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

    let minutesEtc =
        if timeFormat = IncludeMilliseconds then
            let millisecondsPrefix =
                if milliseconds < 10L<millisecond> then "00"
                else if milliseconds < 100L<millisecond> then "0"
                else ""

            $"{minutesPrefix}{minutes}:{secondsPrefix}{seconds}.{millisecondsPrefix}{milliseconds}"
        else
            $"{minutesPrefix}{minutes}:{secondsPrefix}{seconds}"

    match hours with
    | Some hours -> $"{hours}:{minutesEtc}"
    | None -> minutesEtc

let durationText timeFormat =
    function
    | Some duration -> formatTime timeFormat duration
    | None -> "N/A"

let positionText (positionValue: float32) (duration: int64<millisecond> option) =
    match duration with
    | Some duration ->
        let position = Math.Round((positionValue |> double) * (duration |> double)) |> int64
        formatTime NoRounding (position * 1L<millisecond>)
    | None -> "0:00"
