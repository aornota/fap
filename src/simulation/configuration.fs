module Aornota.Fap.Simulation.Configuration

open Aornota.Fap.Simulation.Randomization
open System

// Note: Type aliases defined to allow use of nameof(...) in error messages.
type MaxHealthForNascence = float
type NascenceRateDivisor = int
type MaxAliveWeight = int
type MaxNascentWeight = int
type HorizontalPartitions = int
type VerticalPartitions = int
type RedBias = float
type GreenBias = float
type BlueBias = float
type ExpiresAfter = int

type Fade =
    | Slow
    | Medium
    | Fast

type Configuration =
    { MaxHealthForNascence: MaxHealthForNascence
      NascenceRateDivisor: NascenceRateDivisor
      MaxAliveWeight: MaxAliveWeight
      MaxNascentWeight: MaxNascentWeight
      HorizontalPartitions: HorizontalPartitions
      VerticalPartitions: VerticalPartitions
      RedBias: RedBias
      GreenBias: GreenBias
      BlueBias: BlueBias
      DyingFade: Fade
      DeadFade: Fade
      // TODO-NMB: More configuration?...
      ExpiresAfter: ExpiresAfter option }

[<Literal>]
let private MIN_MAX_HEALTH_FOR_NASCENCE = 0.036

[<Literal>]
let private MAX_MAX_HEALTH_FOR_NASCENCE = 0.108

[<Literal>]
let private MIN_NASCENCE_RATE_DIVISOR = 20

[<Literal>]
let private MAX_NASCENCE_RATE_DIVISOR = 100

[<Literal>]
let private MIN_MAX_ALIVE_WEIGHT = 30

[<Literal>]
let private MAX_MAX_ALIVE_WEIGHT = 70

[<Literal>]
let private MIN_MAX_NASCENT_WEIGHT = 4

[<Literal>]
let private MAX_MAX_NASCENT_WEIGHT = 8

[<Literal>]
let private MIN_PARTITIONS = 2

[<Literal>]
let private MAX_PARTITIONS = 16

[<Literal>]
let private MIN_BIAS = -0.09

[<Literal>]
let private MAX_BIAS = 0.135

[<Literal>]
let private MIN_EXPIRES_AFTER = 100

[<Literal>]
let private MAX_EXPIRES_AFTER = 900

let private maxPartitions (width, height) =
    Math.Min(Math.Max((width / 64) + 1, MIN_PARTITIONS), MAX_PARTITIONS),
    Math.Min(Math.Max((height / 40) + 1, MIN_PARTITIONS), MAX_PARTITIONS)

let private randomFade () =
    match randomNonZero (Some 3) with
    | 1 -> Slow
    | 2 -> Medium
    | 3 -> Fast
    | value -> failwithf $"{nameof (randomNonZero)} ({value}) expected to be 1, 2 or 3"

let private nascence maxHealthForNascence nascenceRateDivisor =
    (maxHealthForNascence / float nascenceRateDivisor) * 10000.

let private minNascence =
    nascence MIN_MAX_HEALTH_FOR_NASCENCE MAX_NASCENCE_RATE_DIVISOR // note: 2.22 (for 0.024 108)

let private maxNascence =
    nascence MAX_MAX_HEALTH_FOR_NASCENCE MIN_NASCENCE_RATE_DIVISOR // note: 33.332 (for 0.12 36)

let nascenceRatio maxHealthForNascence nascenceRateDivisor =
    let nascence = nascence maxHealthForNascence nascenceRateDivisor
    (nascence - minNascence) / (maxNascence - minNascence)

let validate configuration =
    [ if
          configuration.MaxHealthForNascence < MIN_MAX_HEALTH_FOR_NASCENCE
          || configuration.MaxHealthForNascence > MAX_MAX_HEALTH_FOR_NASCENCE
      then
          $"{nameof (MaxHealthForNascence)} {(configuration.MaxHealthForNascence)} must be between {MIN_MAX_HEALTH_FOR_NASCENCE} and {MAX_MAX_HEALTH_FOR_NASCENCE}"

      if
          configuration.NascenceRateDivisor < MIN_NASCENCE_RATE_DIVISOR
          || configuration.NascenceRateDivisor > MAX_NASCENCE_RATE_DIVISOR
      then
          $"{nameof (NascenceRateDivisor)} {(configuration.NascenceRateDivisor)} must be between {MIN_NASCENCE_RATE_DIVISOR} and {MAX_NASCENCE_RATE_DIVISOR}"
      if
          configuration.MaxAliveWeight < MIN_MAX_ALIVE_WEIGHT
          || configuration.MaxAliveWeight > MAX_MAX_ALIVE_WEIGHT
      then
          $"{nameof (MaxAliveWeight)} {(configuration.MaxAliveWeight)} must be between {MIN_MAX_ALIVE_WEIGHT} and {MAX_MAX_ALIVE_WEIGHT}"
      if
          configuration.MaxNascentWeight < MIN_MAX_NASCENT_WEIGHT
          || configuration.MaxNascentWeight > MAX_MAX_NASCENT_WEIGHT
      then
          $"{nameof (MaxNascentWeight)} {(configuration.MaxNascentWeight)} must be between {MIN_MAX_NASCENT_WEIGHT} and {MAX_MAX_NASCENT_WEIGHT}"
      if
          configuration.HorizontalPartitions < MIN_PARTITIONS
          || configuration.HorizontalPartitions > MAX_PARTITIONS
      then
          $"{nameof (HorizontalPartitions)} {(configuration.HorizontalPartitions)} must be between {MIN_PARTITIONS} and {MAX_PARTITIONS}"
      if
          configuration.VerticalPartitions < MIN_PARTITIONS
          || configuration.VerticalPartitions > MAX_PARTITIONS
      then
          $"{nameof (VerticalPartitions)} {(configuration.VerticalPartitions)} must be between {MIN_PARTITIONS} and {MAX_PARTITIONS}"
      if configuration.RedBias < MIN_BIAS || configuration.RedBias > MAX_BIAS then
          $"{nameof (RedBias)} {(configuration.RedBias)} must be between {MIN_BIAS} and {MAX_BIAS}"
      if configuration.BlueBias < MIN_BIAS || configuration.BlueBias > MAX_BIAS then
          $"{nameof (BlueBias)} {(configuration.BlueBias)} must be between {MIN_BIAS} and {MAX_BIAS}"
      if configuration.GreenBias < MIN_BIAS || configuration.GreenBias > MAX_BIAS then
          $"{nameof (GreenBias)} {(configuration.GreenBias)} must be between {MIN_BIAS} and {MAX_BIAS}"
      // TODO-NMB: More configuration validation?...
      match configuration.ExpiresAfter with
      | Some value ->
          if value < MIN_EXPIRES_AFTER || value > MAX_EXPIRES_AFTER then
              $"{nameof (ExpiresAfter)} ({value}) must be between {MIN_EXPIRES_AFTER} and {MAX_EXPIRES_AFTER}"
      | None -> () ]

let randomConfiguration (width, height) =
    let maxHorizontalPartitions, maxVerticalPartitions = maxPartitions (width, height)

    let maxHealthForNascence =
        randomBetween MIN_MAX_HEALTH_FOR_NASCENCE MAX_MAX_HEALTH_FOR_NASCENCE

    let nascenceRateDivisor =
        randomIntBetween MIN_NASCENCE_RATE_DIVISOR MAX_NASCENCE_RATE_DIVISOR

    let expiryAdjustment = 1. - nascenceRatio maxHealthForNascence nascenceRateDivisor

    let expiresAfter =
        int (float (randomIntBetween MIN_EXPIRES_AFTER MAX_EXPIRES_AFTER) * expiryAdjustment)

    { MaxHealthForNascence = maxHealthForNascence
      NascenceRateDivisor = nascenceRateDivisor
      MaxAliveWeight = randomIntBetween MIN_MAX_ALIVE_WEIGHT MAX_MAX_ALIVE_WEIGHT
      MaxNascentWeight = randomIntBetween MIN_MAX_NASCENT_WEIGHT MAX_MAX_NASCENT_WEIGHT
      HorizontalPartitions = randomIntBetween MIN_PARTITIONS maxHorizontalPartitions
      VerticalPartitions = randomIntBetween MIN_PARTITIONS maxVerticalPartitions
      RedBias = randomBetween MIN_BIAS MAX_BIAS
      GreenBias = randomBetween MIN_BIAS MAX_BIAS
      BlueBias = randomBetween MIN_BIAS MAX_BIAS
      DyingFade = randomFade ()
      DeadFade = randomFade ()
      ExpiresAfter = Some(Math.Max(Math.Min(expiresAfter, MAX_EXPIRES_AFTER), MIN_EXPIRES_AFTER)) }
