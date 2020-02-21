module GridTests exposing (all)

import Dict
import Expect
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


all : Test
all =
    describe "Grid generation"
        [ test "Grid.generate generates a Dict of the correct size" <|
            \_ ->
                let
                    size =
                        3

                    bombs =
                        2

                    grid =
                        Grid.generate size bombs (Random.initialSeed 1)
                in
                Expect.equal (Dict.size grid) (size * size)
        , test "Grid.generate generates the correct amount of bombs" <|
            \_ ->
                let
                    size =
                        3

                    bombs =
                        2

                    grid =
                        Grid.generate size bombs (Random.initialSeed 1)
                in
                Expect.equal (Dict.filter isBomb grid |> Dict.size) bombs
        ]
