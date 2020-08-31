module GridTests exposing (all)

import Dict
import Expect
import Fuzz as Fuzz
import Grid
import Random
import Test exposing (..)
import Types exposing (..)


isEmptyCell : Cell -> Bool
isEmptyCell cell =
    case cell of
        Cell _ (AdjacentBombs 0) ->
            True

        _ ->
            False


isBomb : Cell -> Bool
isBomb cell =
    case cell of
        Cell _ Bomb ->
            True

        _ ->
            False


isHidden : ( Coordinates, Cell ) -> Bool
isHidden ( _, cell ) =
    case cell of
        Cell Hidden _ ->
            True

        _ ->
            False


addOne n =
    n + 1


difficultyFuzzer : Fuzz.Fuzzer Difficulty
difficultyFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Easy
        , Fuzz.constant Medium
        , Fuzz.constant Hard
        ]


sizeFuzzer : Fuzz.Fuzzer Int
sizeFuzzer =
    Fuzz.intRange 5 50


all : Test
all =
    describe "Grid generation"
        [ fuzz sizeFuzzer "Grid.generateEmpty generates a Dict of the correct size" <|
            \size ->
                let
                    ( _, grid ) =
                        Grid.generateEmpty size (Random.initialSeed 1)
                in
                Expect.equal (Dict.size grid) (size * size)
        , fuzz sizeFuzzer "Grid.generateEmpty only generates empty cells" <|
            \size ->
                let
                    ( _, grid ) =
                        Grid.generateEmpty size (Random.initialSeed 1)
                in
                Expect.equal True (Dict.values grid |> List.all (\g -> isBomb g |> not))
        , fuzz sizeFuzzer "Provided cell is always empty when generating with generateFromPosition" <|
            \size ->
                let
                    ( seed, grid ) =
                        Grid.generateEmpty size (Random.initialSeed 1)

                    ( _, filledGrid ) =
                        Grid.generateFromPosition grid Easy seed ( 0, 0 )
                in
                Expect.equal 0 (Grid.getAdjacentBombCount ( 0, 0 ) filledGrid)
        ]
