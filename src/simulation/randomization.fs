module Aornota.Fap.Simulation.Randomization

open System
open System.Security.Cryptography

let normalize value = float value / float Int32.MaxValue

let random inclusiveMax =
    match inclusiveMax with
    | Some inclusiveMax when inclusiveMax <= 0 ->
        failwithf $"{nameof (inclusiveMax)} ({inclusiveMax}) must be greater than zero"
    | Some inclusiveMax when inclusiveMax < Int32.MaxValue -> RandomNumberGenerator.GetInt32(int inclusiveMax + 1)
    | Some _
    | None -> RandomNumberGenerator.GetInt32(Int32.MaxValue)

let randomNonZero inclusiveMax =
    match inclusiveMax with
    | Some inclusiveMax when inclusiveMax <= 1 ->
        failwithf $"{nameof (inclusiveMax)} ({inclusiveMax}) must be greater than one"
    | Some inclusiveMax -> random (Some(inclusiveMax - 1)) + 1
    | None -> random None + 1

let normalizedRandom () = normalize (random None)

let normalizedRandomNonZero () =
    let value = randomNonZero None
    normalize (if value < Int32.MaxValue then value else value - 1)

let randomBetween min max =
    if max < min then
        failwithf $"{nameof (max)} ({max}) must not be less than {nameof (min)} ({min})"
    else if max = min then
        max
    else
        min + (normalizedRandom () * (max - min))

let randomIntBetween (min: int) (max: int) =
    if max < min then
        failwithf $"{nameof (max)} ({max}) must not be less than {nameof (min)} ({min})"
    else if max = min then
        max
    else
        min + random (Some(max - min))
