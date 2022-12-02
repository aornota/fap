module Aornota.Fap.Simulation.Model

open Aornota.Fap.Simulation.Configuration

type SimulationSettings =
    { WidthOverride: int option
      HeightOverride: int option
      RenderEveryNOverride: int option }

type CellState =
    | Alive of int * (float * float * float)
    | Dead of int
    | Nascent of int

type Cell = CellState * (byte * byte * byte)

type EvolutionState =
    | Evolving
    | Paused
    | PendingReset
    | Reset
    | InvalidConfiguration

type State =
    { Dimensions: int * int
      Configuration: Configuration * int
      PendingValidateConfiguration: bool
      Generation: int
      Cells: Cell[,]
      Biases: (float * float * float)[,]
      EvolutionState: EvolutionState
      PendingEvolved: bool }
