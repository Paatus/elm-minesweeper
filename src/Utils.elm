module Utils exposing (showAll)

import Dict
import Grid exposing (visit)
import Types exposing (..)


showAll : Grid -> Grid
showAll grid =
    let
        listOfCoords =
            Dict.toList grid
                |> List.map Tuple.first
    in
    List.foldl visit grid listOfCoords
