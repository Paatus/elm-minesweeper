module GridTests exposing (all)

import Dict
import Expect
import Fuzz as Fuzz
import Grid
import Random
import Test exposing (..)
import Types exposing (..)


isBomb : Coordinates -> Cell -> Bool
isBomb mc cell =
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


all : Test
all =
    describe "Grid generation"
        [ fuzz2 difficultyFuzzer (Fuzz.intRange 2 50) "Grid.generate generates a Dict of the correct size" <|
            \diff size ->
                let
                    ( _, grid ) =
                        Grid.generate size diff (Random.initialSeed 1)
                in
                Expect.equal (Dict.size grid) (size * size)
        , test "All cells are hidden upon generation" <|
            \_ ->
                Expect.true "All cells are hidden"
                    (Grid.generate 2 Easy (Random.initialSeed 1)
                        |> Tuple.second
                        |> Dict.toList
                        |> List.all isHidden
                    )
        ]
