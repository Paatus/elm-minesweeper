module Grid exposing (evaluateGameStatus, flag, flagsPlaced, generateEmpty, generateFromPosition, getAdjacentBombCount, getBombAmount, remainingFlags, revealFirstBomb, showSurrounding, untouched, visit)

import Dict
import Random exposing (Seed)
import Random.Extra exposing (filter)
import Set exposing (Set)
import Types exposing (..)



-- Gets the bomb amount based on the difficulty and amount of cells
-- uses the bomb ratio of the original game


getBombAmount : Difficulty -> Int -> Int
getBombAmount difficulty cellAmount =
    let
        factor =
            case difficulty of
                Easy ->
                    10 / (9 * 9)

                Medium ->
                    40 / (16 * 16)

                Hard ->
                    99 / (30 * 16)

        bombs =
            factor * toFloat cellAmount |> floor
    in
    if bombs == 0 then
        bombs + 1

    else
        bombs


swapXY : ( Coordinates, Cell ) -> ( Coordinates, Cell )
swapXY ( ( x, y ), c ) =
    ( ( y, x ), c )


generateEmpty : Int -> Seed -> ( Seed, Grid )
generateEmpty gridSize seed =
    let
        positions =
            generateGrid_ 0 gridSize []

        cells =
            List.repeat (gridSize * gridSize) (Cell Hidden (AdjacentBombs 0))

        newSeed =
            Random.step (Random.int Random.minInt Random.maxInt) seed
                |> Tuple.second
    in
    List.map2 Tuple.pair positions cells
        |> List.map swapXY
        |> Dict.fromList
        |> Tuple.pair newSeed


generateFromPosition : Grid -> Difficulty -> Seed -> Coordinates -> ( Seed, Grid )
generateFromPosition prevGrid difficulty seed coords =
    let
        gridSize =
            size prevGrid

        bombAmount =
            getBombAmount difficulty (gridSize * gridSize)

        newSeed =
            Random.step (Random.int Random.minInt Random.maxInt) seed
                |> Tuple.second

        emptyCoordinates =
            getAdjacentPositions gridSize coords
                |> (\cs -> [ coords ] ++ cs)
                |> Set.fromList
    in
    addRandomMines bombAmount emptyCoordinates ( newSeed, prevGrid )
        |> Tuple.mapSecond (\grid -> Dict.map (\coord -> addAdjacentCounts grid coord) grid)


generateGrid_ : Int -> Int -> List ( Int, Int ) -> List ( Int, Int )
generateGrid_ row maxRows list =
    if row >= maxRows then
        list

    else
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
                Just (Cell _ (AdjacentBombs _)) ->
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


visitCell : Maybe Cell -> Maybe Cell
visitCell mc =
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


expandNearby : Grid -> List Coordinates -> Grid
expandNearby =
    List.foldl visit


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
    in
    case ( adjacentBombs, tileIsBomb ) of
        ( 0, False ) ->
            let
                adjacentCells =
                    getAdjacentPositions (size grid) coords
                        |> List.filter (\x -> isVisited grid x)

                newGrid =
                    Dict.update coords visitCell grid
            in
            expandNearby newGrid adjacentCells

        _ ->
            Dict.update coords visitCell grid


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
    in
    if adjacentBombs == adjacentFlags then
        let
            adjacentCells =
                getAdjacentPositions (size grid) coords
                    |> List.filter (\x -> isVisited grid x)
        in
        expandNearby grid adjacentCells

    else
        grid


visibleBombs : ( Coordinates, Cell ) -> Bool
visibleBombs ( _, cell ) =
    case cell of
        Cell Visible Bomb ->
            True

        _ ->
            False


hasLost : Grid -> Bool
hasLost =
    Dict.toList
        >> List.any visibleBombs


touchedCells : ( Coordinates, Cell ) -> Bool
touchedCells ( _, cell ) =
    case cell of
        Cell Hidden _ ->
            False

        _ ->
            True


remainingFlags : Grid -> Int
remainingFlags grid =
    let
        bombAmount =
            Dict.toList grid
                |> List.filter
                    (\( _, c ) ->
                        case c of
                            Cell _ Bomb ->
                                True

                            _ ->
                                False
                    )
                |> List.length
    in
    Dict.toList grid
        |> List.filter
            (\( _, c ) ->
                case c of
                    Cell Flag _ ->
                        True

                    _ ->
                        False
            )
        |> List.length
        |> (\x -> bombAmount - x)


untouched : Grid -> Bool
untouched =
    Dict.toList
        >> List.any touchedCells
        >> not


hiddenSafeCell : ( Coordinates, Cell ) -> Bool
hiddenSafeCell ( _, cell ) =
    case cell of
        Cell Hidden (AdjacentBombs _) ->
            True

        _ ->
            False


hasWon : Grid -> Bool
hasWon grid =
    let
        gridList =
            Dict.toList grid
    in
    List.any hiddenSafeCell gridList |> not


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


showCell : Grid -> Coordinates -> Grid
showCell grid coords =
    Dict.update coords
        (\mc ->
            case mc of
                Just (Cell _ state) ->
                    Just (Cell Visible state)

                Nothing ->
                    Nothing
        )
        grid


revealFirstBomb : Grid -> Maybe ( Grid, Bool )
revealFirstBomb grid =
    let
        cell =
            Dict.toList grid
                |> List.filter
                    (\( coords, c ) ->
                        case c of
                            Cell Hidden Bomb ->
                                True

                            Cell Flag Bomb ->
                                True

                            _ ->
                                False
                    )
                |> List.head
    in
    case cell of
        Nothing ->
            Nothing

        Just ( co, ce ) ->
            let
                nextGrid =
                    showCell grid co
            in
            Just ( nextGrid, True )
