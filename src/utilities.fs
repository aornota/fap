module Aornota.Fap.Utilities

open System

[<Measure>]
type millisecond

let private formatHoursMinutesAndSeconds (value: int64<millisecond>) =
    let second = 1000L<millisecond>
    let minute = second * 60L
    let hour = minute * 60L
    let hours = if value > hour then Some(value / hour) else None

    let value =
        match hours with
        | Some hours -> value - (hours * hour)
        | None -> value

    let minutes = if value > minute then value / minute else 0
    let value = value - (minutes * minute)
    // TODO-NMB: Finesse handling of seconds (since 59->00 longer than 00->01)?...
    let seconds = if value > second then value / second else 0
    let milliseconds = value - (seconds * second)

    let seconds =
        if milliseconds > 500L<millisecond> && seconds < 59L then
            seconds + 1L
        else
            seconds

    let minutesPrefix = if minutes < 10 then "0" else ""
    let secondsPrefix = if seconds < 10 then "0" else ""
    let minutesAndSeconds = $"{minutesPrefix}{minutes}:{secondsPrefix}{seconds}"

    match hours with
    | Some hours -> $"{hours}:{minutesAndSeconds}"
    | None -> minutesAndSeconds

let durationText =
    function
    | Some duration -> formatHoursMinutesAndSeconds duration
    | None -> "N/A"

let positionText (positionValue: float32) (duration: int64<millisecond> option) =
    match duration with
    | Some duration ->
        let position = Math.Round((positionValue |> double) * (duration |> double)) |> int64
        formatHoursMinutesAndSeconds (position * 1L<millisecond>)
    | None -> "0:00"
