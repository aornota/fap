module Aornota.Fap.Simulation.Transition

open Aornota.Fap.Simulation.Configuration
open Aornota.Fap.Simulation.Model
open Aornota.Fap.Simulation.Randomization
open Aornota.Fap.Persistence
open Aornota.Fap.Utilities
open Elmish
open System
open System.Drawing

type ExternalMsg = NotifyError of string * string option

type Msg =
    // Internal
    | RequestEvolve
    | Evolved of int * (Cell[,] * (float * float * float)[,])
    // From Playlists
    | OnEvolve
    | OnPause
    | OnReset
    | OnRandomConfiguration

[<Literal>]
let private SIMULATION_WIDTH = 112

[<Literal>]
let private SIMULATION_HEIGHT = 112

[<Literal>]
let private INITIAL_GENERATION = 0

[<Literal>]
let private RENDER_EVERY_N: byte = 1uy

[<Literal>]
let private EVOLVE_DELAY: byte = 1uy

let private rgbMin, rgbMax = Byte.MinValue, Byte.MaxValue

let private black = (rgbMin, rgbMin, rgbMin)

let private initBlack (width, height) =
    Array2D.create width height (Dead 0, black)

let private bias offset =
    ((normalizedRandom () / 4.) - 0.1) + offset

let private partitionWidthAndHeight xPartitions yPartitions (width, height) =
    width / xPartitions, height / yPartitions

let private partitionIndices (partitionWidth, partitionHeight) xPartitions yPartitions x y =
    Math.Min(x / partitionWidth, xPartitions - 1), Math.Min(y / partitionHeight, yPartitions - 1)

let private addNascent dimensions configuration (cells: Cell[,]) =
    let maxNascentWeight = configuration.MaxNascentWeight

    let xPartitions, yPartitions =
        configuration.HorizontalPartitions, configuration.VerticalPartitions

    let width, height = dimensions

    let partitionWidth, partitionHeight =
        partitionWidthAndHeight xPartitions yPartitions dimensions

    let halfPartitionWidth, halfPartitionHeight =
        partitionWidth / 2, partitionHeight / 2

    let partitionCapacity = partitionWidth * partitionHeight

    let partitionIndices =
        partitionIndices (partitionWidth, partitionHeight) xPartitions yPartitions

    let mutable aliveAndNascentWeights = []

    cells
    |> Array2D.iteri (fun x y cell ->
        match fst cell with
        | Alive (weight, _) ->
            aliveAndNascentWeights <- (partitionIndices x y, Some weight, None) :: aliveAndNascentWeights
        | Nascent weight ->
            aliveAndNascentWeights <- (partitionIndices x y, None, Some weight) :: aliveAndNascentWeights
        | _ -> ())

    let aliveCountsAndNascentWeightedCounts =
        aliveAndNascentWeights
        |> List.groupBy (fun (indices, _, _) -> indices)
        |> List.map (fun (indices, values) ->
            let aliveCount =
                values |> List.choose (fun (_, aliveWeight, _) -> aliveWeight) |> List.length

            let nascentWeightedCount =
                values
                |> List.choose (fun (_, _, nascentWeight) -> nascentWeight)
                |> List.sumBy (fun nascentWeight ->
                    float ((maxNascentWeight + 1) - nascentWeight) * (1. / float maxNascentWeight))

            indices, aliveCount, nascentWeightedCount)

    let nascentRate population =
        let health = population / float partitionCapacity

        if health < configuration.MaxHealthForNascence then
            Some((1. - health) / float configuration.NascenceRateDivisor)
        else
            None

    let nascentRates = Array2D.create xPartitions yPartitions (nascentRate 0.)

    aliveCountsAndNascentWeightedCounts
    |> List.iter (fun ((x, y), aliveCount, nascentWeightedCount) ->
        nascentRates[x, y] <- nascentRate (float aliveCount - nascentWeightedCount))

    cells
    |> Array2D.mapi (fun x y cell ->
        match cell with
        | Dead _, (r, g, b) ->
            let x, y =
                (x + random (Some halfPartitionWidth)) % width, (y + random (Some halfPartitionHeight)) % height

            let xPartition, yPartition = partitionIndices x y

            match nascentRates[xPartition, yPartition] with
            | Some nascentRate when normalizedRandom () < nascentRate -> Nascent 1, (r, g, b)
            | _ -> cell
        | _ -> cell)

let private evolve dimensions configuration (cells: Cell[,]) (biases: (float * float * float)[,]) =
    // Based on https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life.

    // TODO-NMB: Make (some of) these configurable?...
    let newBiasThreshhold = 0.012
    let mightSurviveIfAliveCountLessThanA, mightSurviveIfAliveCountLessThanB = 2, 4
    let senescenceWeightDivisor = 2.
    let senescenceRateDivisorA, senescenceRateDivisorB = 750., 1000.
    let aliveWeightGainA, aliveWeightGainB = 1, 2
    let aliveBaseWeightingA, aliveBaseWeightingB = 0.8, 1.2
    let aliveBrightenMultiplierA, aliveBrightenMultiplierB = 7, 15
    let resurrectIfAliveCountEqualsAndNascentCountLessThan = (3, 2)
    let resurrectionWeightGain, resurrectionBaseWeighting = 1, 0.6
    let bornIfAliveCountPLusNascentCountMoreThan = 2
    let birthWeight, birthBaseWeighting = 1, 0.9
    let nascentWeightGain = 1

    let maxAliveWeight, maxNascentWeight =
        configuration.MaxAliveWeight, configuration.MaxNascentWeight

    let xPartitions, yPartitions =
        configuration.HorizontalPartitions, configuration.VerticalPartitions

    let width, height = dimensions

    let partitionWidth, partitionHeight =
        partitionWidthAndHeight xPartitions yPartitions dimensions

    let halfPartitionWidth, halfPartitionHeight =
        partitionWidth / 2, partitionHeight / 2

    let partitionIndices =
        partitionIndices (partitionWidth, partitionHeight) xPartitions yPartitions

    let maxX, maxY = width - 1, height - 1

    let neighbourData x y =
        let left = if x > 0 then x - 1 else maxX
        let right = if x < maxX then x + 1 else 0
        let above = if y > 0 then y - 1 else maxY
        let below = if y < maxY then y + 1 else 0

        let neighbours =
            [ cells.[left, above]
              cells.[x, above]
              cells.[right, above]
              cells.[left, y]
              cells.[right, y]
              cells.[left, below]
              cells.[x, below]
              cells.[right, below] ]

        let data =
            neighbours
            |> List.choose (fun cell ->
                match cell with
                | Alive (_, (rSeed, gSeed, bSeed)), _ -> Some(true, Some(rSeed, gSeed, bSeed))
                | Dead _, _ -> None
                | Nascent _, _ -> Some(false, None))

        let aliveCount = data |> List.filter fst |> List.length
        let nascentCount = data |> List.filter (fst >> not) |> List.length
        let seeds = data |> List.choose snd

        let meanSeeds =
            match seeds.Length with
            | 0 -> None
            | length ->
                let rMean = (seeds |> List.sumBy (fun (r, _, _) -> r)) / float length
                let gMean = (seeds |> List.sumBy (fun (_, g, _) -> g)) / float length
                let bMean = (seeds |> List.sumBy (fun (_, _, b) -> b)) / float length
                Some(rMean, gMean, bMean)

        aliveCount, nascentCount, meanSeeds

    let seeds (rSeed: float, gSeed: float, bSeed: float) meanSeeds baseWeighting =
        match meanSeeds with
        | Some (rOther, gOther, bOther) ->
            let divisor = baseWeighting + 1.
            let rSeed = ((rSeed * baseWeighting) + rOther) / divisor
            let gSeed = ((gSeed * baseWeighting) + gOther) / divisor
            let bSeed = ((bSeed * baseWeighting) + bOther) / divisor
            rSeed, gSeed, bSeed
        | None -> rSeed, gSeed, bSeed

    let brighten (r, g, b) (rSeed, gSeed, bSeed) multiplier =
        let multiplier = float multiplier

        if r < rgbMax && g < rgbMax && b < rgbMax then
            byte (Math.Max(Math.Min(int r + (int (rSeed * multiplier)), int rgbMax), int rgbMin)),
            byte (Math.Max(Math.Min(int g + (int (gSeed * multiplier)), int rgbMax), int rgbMin)),
            byte (Math.Max(Math.Min(int b + (int (bSeed * multiplier)), int rgbMax), int rgbMin))
        else
            r, g, b

    let biases =
        biases
        |> Array2D.map (fun (rBias, gBias, bBias) ->
            if normalizedRandom () < newBiasThreshhold then
                let threshhold = 0.33

                (if normalizedRandom () < threshhold then
                     bias configuration.RedBias
                 else
                     rBias),
                (if normalizedRandom () < threshhold then
                     bias configuration.GreenBias
                 else
                     gBias),
                if normalizedRandom () < threshhold then
                    bias configuration.BlueBias
                else
                    bBias
            else
                rBias, gBias, bBias)

    let randomSeeds x y =
        let x, y =
            (x + random (Some halfPartitionWidth)) % width, (y + random (Some halfPartitionHeight)) % height

        let xPartition, yPartition = partitionIndices x y

        let rBias, gBias, bBias = biases.[xPartition, yPartition]

        normalizedRandom () + rBias, normalizedRandom () + gBias, normalizedRandom () + bBias

    let dyingFade weight =
        match configuration.DyingFade with
        | Slow -> int (Math.Sqrt(Math.Sqrt(int weight)))
        | Medium -> int (Math.Sqrt(Math.Sqrt(int weight)) * (if weight >= 30 then 2. else 1.5))
        | Fast -> int (Math.Sqrt(int weight))

    let senescenceThreshhold =
        int (float configuration.MaxAliveWeight / senescenceWeightDivisor)

    let cells =
        cells
        |> addNascent dimensions configuration
        |> Array2D.mapi (fun x y cell ->
            match cell, neighbourData x y with
            | (Alive (weight, (rSeed, gSeed, bSeed)), (r, g, b)), (aliveCount, nascentCount, meanSeeds) when
                aliveCount < mightSurviveIfAliveCountLessThanA
                ->
                if
                    nascentCount >= ((mightSurviveIfAliveCountLessThanA + 1) - aliveCount)
                    && (weight <= senescenceThreshhold
                        || normalizedRandom () > float (weight - senescenceThreshhold) / senescenceRateDivisorA)
                then
                    Alive(
                        Math.Min(weight + aliveWeightGainA, maxAliveWeight),
                        seeds (rSeed, gSeed, bSeed) meanSeeds aliveBaseWeightingA
                    ),
                    (brighten (r, g, b) (rSeed, gSeed, bSeed) aliveBrightenMultiplierA)
                else
                    Dead(weight - dyingFade weight), (r, g, b)
            | (Alive (weight, (rSeed, gSeed, bSeed)), (r, g, b)), (aliveCount, nascentCount, meanSeeds) when
                aliveCount < mightSurviveIfAliveCountLessThanB
                ->
                if
                    nascentCount <= (mightSurviveIfAliveCountLessThanB - aliveCount)
                    && (weight <= senescenceThreshhold
                        || normalizedRandom () > float (weight - senescenceThreshhold) / senescenceRateDivisorB)
                then
                    Alive(
                        Math.Min(weight + aliveWeightGainB, maxAliveWeight),
                        seeds (rSeed, gSeed, bSeed) meanSeeds aliveBaseWeightingB
                    ),
                    (brighten (r, g, b) (rSeed, gSeed, bSeed) aliveBrightenMultiplierB)
                else
                    Dead(weight - dyingFade weight), (r, g, b)
            | (Alive (weight, _), (r, g, b)), _ -> Dead(weight - dyingFade weight), (r, g, b)
            | (Dead weight, (r, g, b)), (aliveCount, nascentCount, meanSeeds) when
                aliveCount = fst resurrectIfAliveCountEqualsAndNascentCountLessThan
                && nascentCount < snd resurrectIfAliveCountEqualsAndNascentCountLessThan
                ->
                Alive(
                    Math.Min(weight + resurrectionWeightGain, maxAliveWeight),
                    seeds (randomSeeds x y) meanSeeds resurrectionBaseWeighting
                ),
                (r, g, b)
            | (Dead weight, (r, g, b)), _ ->
                let weight, r, g, b =
                    if weight > 0 then
                        weight - dyingFade weight, r, g, b
                    else
                        let max = Math.Max(r, Math.Max(g, b))

                        let threshhold, maxAmount =
                            match configuration.DeadFade with
                            | Slow ->
                                Math.Sqrt(Math.Sqrt(float max)) * (if max >= 81uy then 1.5 else 1.25),
                                (max / 40uy) + 1uy
                            | Medium ->
                                Math.Sqrt(Math.Sqrt(float max)) * (if max >= 81uy then 2. else 1.75), (max / 28uy) + 1uy
                            | Fast -> Math.Sqrt(float max), (max / 16uy) + 1uy

                        let fade =
                            match normalizedRandom () < (threshhold / 17.), maxAmount with
                            | true, 1uy -> 1
                            | true, maxAmount -> randomNonZero (Some(int maxAmount))
                            | _ -> 0

                        weight,
                        byte (Math.Max(int r - fade, 0)),
                        byte (Math.Max(int g - fade, 0)),
                        byte (Math.Max(int b - fade, 0))

                Dead weight, (r, g, b)
            | (Nascent _, (r, g, b)), (aliveCount, nascentCount, meanSeeds) when
                aliveCount + nascentCount > bornIfAliveCountPLusNascentCountMoreThan
                ->
                Alive(birthWeight, seeds (randomSeeds x y) meanSeeds birthBaseWeighting), (r, g, b)
            | (Nascent weight, (r, g, b)), _ when weight < maxNascentWeight ->
                Nascent(weight + nascentWeightGain), (r, g, b)
            | (Nascent _, (r, g, b)), _ -> Dead 0, (r, g, b))

    cells, biases

let private evolveAsync state =
    let dimensions, configuration = state.Dimensions, fst state.Configuration

    let renderEvery = int (Math.Min(RENDER_EVERY_N, 1uy))

    let rec evolveN n (cells, biases) =
        if n > 0 then
            evolveN (n - 1) (evolve dimensions configuration cells biases)
        else
            cells, biases

    async {
        do! Async.Sleep(int EVOLVE_DELAY)
        return state.Generation + renderEvery, evolveN renderEvery (state.Cells, state.Biases)
    }

let private asBitmap state =
    let toColour x y =
        match state.Cells[x, y] with
        | _, (r, g, b) -> Color.FromArgb(0, int r, int g, int b)

    initArray state.Cells toColour |> toBitmap

let asAvaloniaBitmap state = state |> asBitmap |> toAvaloniaBitmap

let init () =
    let dimensions = SIMULATION_WIDTH, SIMULATION_HEIGHT

    { Dimensions = dimensions
      Configuration = (randomConfiguration dimensions, INITIAL_GENERATION)
      PendingValidateConfiguration = true
      Generation = INITIAL_GENERATION
      Cells = initBlack dimensions
      Biases = Array2D.create 0 0 (0., 0., 0.)
      EvolutionState = Reset
      PendingEvolved = false }

let update msg (state: State) =
    match msg with
    // Internal
    | RequestEvolve ->
        let validationErrors, biases =
            if state.PendingValidateConfiguration then
                let configuration, _ = state.Configuration

                match validate configuration with
                | [] ->
                    let biases =
                        Array2D.init configuration.HorizontalPartitions configuration.VerticalPartitions (fun _ _ ->
                            bias configuration.RedBias, bias configuration.GreenBias, bias configuration.BlueBias)

                    [], Some biases
                | errors -> errors, None
            else
                [], None

        let newState =
            { state with
                PendingValidateConfiguration = false
                Biases = biases |> Option.defaultValue state.Biases }

        let newState, cmd, externalMsg =
            match validationErrors with
            | _ :: _ ->
                let externalMsgs =
                    [ NotifyError(
                          String.concat Environment.NewLine validationErrors,
                          Some $"Invalid {nameof (Configuration)}"
                      ) ]

                { newState with
                    EvolutionState = InvalidConfiguration
                    PendingEvolved = false },
                Cmd.none,
                externalMsgs
            | [] ->
                let newState =
                    { newState with
                        EvolutionState = Evolving
                        PendingEvolved = true }

                newState, Cmd.OfAsync.perform evolveAsync newState Evolved, []

        newState, cmd, externalMsg
    | Evolved (generation, (cells, biases)) ->
        if state.EvolutionState = PendingReset then
            { state with
                EvolutionState = Reset
                PendingEvolved = false },
            Cmd.none,
            []
        else
            let newConfiguration =
                match
                    state.PendingValidateConfiguration, (fst state.Configuration).ExpiresAfter, snd state.Configuration
                with
                | false, Some value, fromGeneration ->
                    let value = fromGeneration + value

                    if generation < value then
                        None
                    else
                        Some(randomConfiguration state.Dimensions, generation)
                | _ -> None

            { state with
                Configuration = newConfiguration |> Option.defaultValue state.Configuration
                PendingValidateConfiguration =
                    if newConfiguration |> Option.isSome then
                        true
                    else
                        state.PendingValidateConfiguration
                Generation = generation
                Cells = cells
                Biases = biases
                PendingEvolved = false },
            (if state.EvolutionState = Evolving then
                 Cmd.ofMsg RequestEvolve
             else
                 Cmd.none),
            []
    // From Playists
    | OnEvolve ->
        match state.EvolutionState with
        | Paused
        | Reset -> { state with EvolutionState = Evolving }, Cmd.ofMsg RequestEvolve, []
        | InvalidConfiguration -> state, Cmd.batch [ Cmd.ofMsg OnRandomConfiguration; Cmd.ofMsg RequestEvolve ], []
        | _ ->
            state,
            Cmd.none,
            [ NotifyError($"{nameof (OnEvolve)} when {nameof (EvolutionState)} is {state.EvolutionState}", None) ]
    | OnPause ->
        if state.EvolutionState = Evolving then
            { state with EvolutionState = Paused }, Cmd.none, []
        else
            state,
            Cmd.none,
            [ NotifyError($"{nameof (OnPause)} when {nameof (EvolutionState)} is {state.EvolutionState}", None) ]
    | OnReset ->
        match state.EvolutionState with
        | Evolving
        | Paused ->
            let configuration, fromGeneration = state.Configuration

            { state with
                Configuration = (configuration, fromGeneration - state.Generation)
                Generation = INITIAL_GENERATION
                Cells = initBlack state.Dimensions
                EvolutionState = if state.PendingEvolved then PendingReset else Reset },
            Cmd.none,
            []
        | _ ->
            state,
            Cmd.none,
            [ NotifyError($"{nameof (OnReset)} when {nameof (EvolutionState)} is {state.EvolutionState}", None) ]
    | OnRandomConfiguration ->
        { state with
            Configuration = (randomConfiguration state.Dimensions, state.Generation)
            PendingValidateConfiguration = true },
        Cmd.none,
        []
