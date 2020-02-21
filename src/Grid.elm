module Grid exposing (evaluateGameStatus, flag, flagsPlaced, generate, placedFlags, showSurrounding, untouched, visit)

import Dict exposing (Dict)
import Random exposing (Seed)
import Random.Extra exposing (filter)
import Set exposing (Set)
import Types exposing (..)


generate : Int -> Int -> Seed -> Grid
generate gridSize bombAmount seed =
    let
        positions =
            generateGrid_ 0 gridSize []

        cells =
            List.repeat (gridSize * gridSize) (Cell Hidden (AdjacentBombs 0))
    in
    List.map2 Tuple.pair positions cells
        |> Dict.fromList
        |> (\g -> addRandomMines bombAmount Set.empty ( seed, g ))
        |> Tuple.second
        |> (\grid -> Dict.map (\coord -> addAdjacentCounts grid coord) grid)


generateGrid_ : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
generateGrid_ row maxRows list =
    case row >= maxRows of
        True ->
            list

        False ->
            let
                newSpots =
                    generateRow row maxRows

                newList =
                    list ++ newSpots
            in
            generateGrid_ (row + 1) maxRows newList


generateRow : Int -> Int -> List ( Int, Int )
generateRow x rowSize =
    let
        xs =
            List.repeat rowSize x

        ys =
            List.range 0 (rowSize - 1)
    in
    List.map2 Tuple.pair xs ys


addRandomMines : Int -> Set ( Int, Int ) -> ( Seed, Grid ) -> ( Seed, Grid )
addRandomMines numMines_ excludedCoords ( seed, grid ) =
    List.foldl
        (always (addRandomMine excludedCoords))
        ( seed, grid )
        (List.repeat numMines_ ())


addRandomMine : Set ( Int, Int ) -> ( Seed, Grid ) -> ( Seed, Grid )
addRandomMine excludedCoords ( seed, grid ) =
    let
        isAvailable ( xP, yP ) =
            case Dict.get ( xP, yP ) grid of
                Just (Cell Hidden (AdjacentBombs _)) ->
                    not (Set.member ( xP, yP ) excludedCoords)

                _ ->
                    False

        xGenerator =
            Random.int 0 (size grid)

        yGenerator =
            Random.int 0 (size grid)

        coordsGenerator =
            Random.pair xGenerator yGenerator
                |> filter isAvailable

        ( ( x, y ), newSeed ) =
            Random.step coordsGenerator seed

        newGrid =
            Dict.update ( x, y ) setBombCell grid
    in
    ( newSeed, newGrid )


size : Grid -> Int
size grid =
    sqrt (Dict.size grid |> toFloat) |> round


setBombCell : Maybe Cell -> Maybe Cell
setBombCell mc =
    Just (Cell Hidden Bomb)


setVisibleCell : Maybe Cell -> Maybe Cell
setVisibleCell mc =
    case mc of
        Just (Cell Hidden a) ->
            Just (Cell Visible a)

        _ ->
            mc


toggleFlagCell : Maybe Cell -> Maybe Cell
toggleFlagCell mc =
    case mc of
        Just (Cell Hidden a) ->
            Just (Cell Flag a)

        Just (Cell Flag a) ->
            Just (Cell Hidden a)

        _ ->
            mc


setAdjacentBombs : Int -> Maybe Cell -> Maybe Cell
setAdjacentBombs bombAmount mc =
    Just (Cell Hidden (AdjacentBombs bombAmount))


insideGrid : Int -> Coordinates -> Bool
insideGrid maxSize ( x, y ) =
    (x >= 0 && x < maxSize) && (y >= 0 && y < maxSize)


getAdjacentPositions : Int -> Coordinates -> List Coordinates
getAdjacentPositions maxSize ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]
        |> List.filter (insideGrid maxSize)


addAdjacentCounts : Grid -> Coordinates -> Cell -> Cell
addAdjacentCounts grid coords cell =
    case cell of
        Cell Hidden (AdjacentBombs _) ->
            let
                bombCount =
                    getAdjacentBombCount coords grid
            in
            Cell Hidden (AdjacentBombs bombCount)

        _ ->
            cell


isBomb : Grid -> Coordinates -> Bool
isBomb grid coords =
    case Dict.get coords grid of
        Just (Cell _ Bomb) ->
            True

        _ ->
            False


isFlag : Grid -> Coordinates -> Bool
isFlag grid coords =
    case Dict.get coords grid of
        Just (Cell Flag _) ->
            True

        _ ->
            False


getAdjacentBombCount : Coordinates -> Grid -> Int
getAdjacentBombCount pos grid =
    let
        adjacentPositions =
            getAdjacentPositions (size grid) pos
    in
    List.filter (\x -> isBomb grid x) adjacentPositions
        |> List.length


getAdjacentFlagCount : Coordinates -> Grid -> Int
getAdjacentFlagCount pos grid =
    let
        adjacentPositions =
            getAdjacentPositions (size grid) pos
    in
    List.filter (\x -> isFlag grid x) adjacentPositions
        |> List.length


expandNearby : List Coordinates -> Grid -> Grid
expandNearby coords grid =
    case coords of
        first :: rest ->
            expandNearby rest (visit first grid)

        [] ->
            grid


isVisited : Grid -> Coordinates -> Bool
isVisited grid coords =
    case Dict.get coords grid of
        Just (Cell Visible _) ->
            False

        _ ->
            True


visit : Coordinates -> Grid -> Grid
visit coords grid =
    let
        adjacentBombs =
            getAdjacentBombCount coords grid

        tileIsBomb =
            isBomb grid coords

        adjacentCells =
            getAdjacentPositions (size grid) coords
                |> List.filter (\x -> isVisited grid x)
    in
    case ( adjacentBombs, tileIsBomb ) of
        ( 0, False ) ->
            Dict.update coords setVisibleCell grid
                |> expandNearby adjacentCells

        ( _, True ) ->
            Dict.update coords setVisibleCell grid

        _ ->
            Dict.update coords setVisibleCell grid


flag : Coordinates -> Grid -> Grid
flag coords grid =
    Dict.update coords toggleFlagCell grid


showSurrounding : Coordinates -> Grid -> Grid
showSurrounding coords grid =
    let
        adjacentBombs =
            getAdjacentBombCount coords grid

        adjacentFlags =
            getAdjacentFlagCount coords grid

        adjacentCells =
            getAdjacentPositions (size grid) coords
                |> List.filter (\x -> isVisited grid x)
    in
    case adjacentBombs == adjacentFlags of
        True ->
            expandNearby adjacentCells grid

        False ->
            grid


visibleBombs : Coordinates -> Cell -> Bool
visibleBombs _ cell =
    case cell of
        Cell Visible Bomb ->
            True

        _ ->
            False


gt : Int -> Int -> Bool
gt less more =
    less < more


hasLost : Grid -> Bool
hasLost grid =
    Dict.filter visibleBombs grid
        |> Dict.size
        |> gt 0


touchedCells : Coordinates -> Cell -> Bool
touchedCells _ cell =
    case cell of
        Cell Hidden _ ->
            False

        _ ->
            True


placedFlags : Grid -> Int
placedFlags grid =
    Dict.filter
        (\_ c ->
            case c of
                Cell Flag _ ->
                    True

                _ ->
                    False
        )
        grid
        |> Dict.size


untouched : Grid -> Bool
untouched grid =
    Dict.filter touchedCells grid
        |> Dict.size
        |> (==) 0


nonFlaggedBombs : Coordinates -> Cell -> Bool
nonFlaggedBombs _ cell =
    case cell of
        Cell Flag Bomb ->
            False

        Cell _ Bomb ->
            True

        _ ->
            False


hiddenSafeCell : Coordinates -> Cell -> Bool
hiddenSafeCell _ cell =
    case cell of
        Cell Hidden (AdjacentBombs _) ->
            True

        _ ->
            False


hasWon : Grid -> Bool
hasWon grid =
    let
        bombsWithoutFlags =
            Dict.filter nonFlaggedBombs grid
                |> Dict.size

        safeCellsUnrevealed =
            Dict.filter hiddenSafeCell grid
                |> Dict.size
    in
    case ( safeCellsUnrevealed, bombsWithoutFlags ) of
        ( 0, _ ) ->
            True

        ( _, 0 ) ->
            True

        _ ->
            False


evaluateGameStatus : Grid -> GameStatus
evaluateGameStatus grid =
    let
        lost =
            hasLost grid

        won =
            hasWon grid
    in
    case ( lost, won ) of
        ( True, _ ) ->
            Lost

        ( _, True ) ->
            Won

        _ ->
            Running


flagsPlaced : Grid -> Int
flagsPlaced grid =
    Dict.filter
        (\coords cell ->
            case cell of
                Cell Flag _ ->
                    True

                _ ->
                    False
        )
        grid
        |> Dict.size
