module Types exposing (..)

import Dict exposing (Dict)


type Difficulty
    = Easy
    | Medium
    | Hard


type alias Grid =
    Dict Coordinates Cell


type Cell
    = Cell CellState CellInner


type CellState
    = Hidden
    | Visible
    | Flag


type CellInner
    = Bomb
    | AdjacentBombs Int


type alias Coordinates =
    ( Int, Int )


type GameStatus
    = Running
    | Lost
    | Won
