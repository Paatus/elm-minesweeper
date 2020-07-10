module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Element exposing (Element, alignLeft, alignRight, centerX, centerY, column, el, fill, height, htmlAttribute, image, layout, padding, paddingEach, paddingXY, px, rgb255, row, spaceEvenly, spacing, text, width, wrappedRow)
import Element.Background as Bg
import Element.Border as B
import Element.Events exposing (onClick)
import Element.Font as F exposing (bold, heavy)
import Element.Input as I exposing (button)
import Element.Keyed as Keyed
import Element.Lazy exposing (lazy)
import Grid
import Html as Html
import Html.Attributes as HA
import Json.Decode as Json
import Process as Process
import Random exposing (Generator)
import RightClick exposing (onRightClick)
import Svg
import Svg.Attributes as SVGA
import Svg.Events as SVGE
import Task as Task
import Time exposing (every, toMillis, utc)
import Types exposing (..)
import Utils exposing (showAll)



---- MODEL ----


type GameState
    = StartMenu
    | InGame


type alias Model =
    { randomSeed : Random.Seed
    , gameState : GameState
    , grid : Grid
    , gameStatus : GameStatus
    , gridSize : Int
    , remainingFlags : Int
    , gameDurationSeconds : Int
    , difficulty : Difficulty
    , minGridSize : Int
    , maxGridSize : Int
    }


init : Int -> ( Model, Cmd Msg )
init initialSeedNumber =
    let
        initialSeed =
            Random.initialSeed initialSeedNumber

        gridSize =
            20

        difficulty =
            Easy

        ( seed, grid ) =
            Grid.generateEmpty gridSize initialSeed
    in
    ( { randomSeed = seed
      , gameState = StartMenu
      , grid = grid
      , gameStatus = Running
      , gridSize = gridSize
      , remainingFlags = Grid.remainingFlags grid
      , gameDurationSeconds = 0
      , difficulty = difficulty
      , minGridSize = 2
      , maxGridSize = 50
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | StartGame
    | ResetGame
    | BackToMenu
    | CellClick Coordinates
    | CellRightClick Coordinates
    | VisibleCellClick Coordinates
    | SecondElapsed Time.Posix
    | SetGridSize Int
    | SetDifficulty Difficulty
    | RevealBomb


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SecondElapsed _ ->
            ( tickSeconds model, Cmd.none )

        StartGame ->
            let
                ( seed, grid ) =
                    Grid.generateEmpty model.gridSize model.randomSeed
            in
            ( { model
                | gameState = InGame
                , grid = grid
                , remainingFlags = Grid.remainingFlags grid
                , randomSeed = seed
              }
            , Cmd.none
            )

        ResetGame ->
            updateGridAndStatus (\_ -> Grid.generateEmpty model.gridSize model.randomSeed)
                { model
                    | gameDurationSeconds = 0
                }

        CellClick ( x, y ) ->
            let
                g =
                    if isFirstAction model.grid then
                        Grid.generateFromPosition model.grid model.difficulty model.randomSeed ( x, y )
                            |> Tuple.second

                    else
                        model.grid
            in
            updateGridAndStatus
                (Grid.visit ( x, y )
                    >> Tuple.pair model.randomSeed
                )
                { model | grid = g }

        VisibleCellClick ( x, y ) ->
            updateGridAndStatus
                (Grid.showSurrounding ( x, y )
                    >> Tuple.pair model.randomSeed
                )
                model

        CellRightClick ( x, y ) ->
            updateGridAndStatus
                (Grid.flag ( x, y )
                    >> Tuple.pair model.randomSeed
                )
                model

        BackToMenu ->
            update ResetGame { model | gameState = StartMenu }

        SetGridSize size ->
            ( { model | gridSize = size }, Cmd.none )

        SetDifficulty difficulty ->
            update ResetGame { model | difficulty = difficulty }

        RevealBomb ->
            revealBombs model.grid model


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.perform (\_ -> msg)


tickSeconds : Model -> Model
tickSeconds model =
    { model | gameDurationSeconds = model.gameDurationSeconds + 1 }


isFirstAction : Grid -> Bool
isFirstAction grid =
    Grid.untouched grid


revealBombs : Grid -> Model -> ( Model, Cmd Msg )
revealBombs grid model =
    case Grid.revealFirstBomb grid of
        Just ( nGrid, _ ) ->
            ( { model | grid = nGrid }, delay 0.0 RevealBomb )

        Nothing ->
            ( model, Cmd.none )


updateGridAndStatus : (Grid -> ( Random.Seed, Grid )) -> Model -> ( Model, Cmd Msg )
updateGridAndStatus gridUpdater model =
    let
        ( newSeed, newGrid ) =
            gridUpdater model.grid

        gameStatus =
            Grid.evaluateGameStatus newGrid

        remainingFlags =
            Grid.remainingFlags newGrid

        newModel =
            { model
                | grid = newGrid
                , gameStatus = gameStatus
                , remainingFlags = remainingFlags
                , randomSeed = newSeed
            }
    in
    if gameStatus == Lost then
        revealBombs newGrid newModel

    else
        ( newModel
        , Cmd.none
        )



---- VIEW ----


numToColor : Int -> String
numToColor n =
    case n of
        1 ->
            "blue"

        2 ->
            "green"

        3 ->
            "red"

        4 ->
            "darkblue"

        5 ->
            "darkred"

        6 ->
            "cyan"

        7 ->
            "black"

        8 ->
            "gray"

        _ ->
            "black"


maybeSet : (Int -> Msg) -> String -> Msg
maybeSet msg x =
    case String.toInt x of
        Nothing ->
            msg 0

        Just b ->
            msg b


viewDifficultySelect : Difficulty -> Element Msg
viewDifficultySelect selectedDifficulty =
    let
        styles diff =
            let
                selectedStyles =
                    if diff == selectedDifficulty then
                        [ Bg.color (rgb255 18 147 216)
                        , F.color (rgb255 255 255 255)
                        , Element.mouseOver [ Bg.color (rgb255 68 197 255) ]
                        ]

                    else
                        [ Bg.color (rgb255 220 220 220)
                        , F.color (rgb255 0 0 0)
                        ]
            in
            [ width fill
            , centerY
            , centerX
            , F.center
            , padding 20
            , noOutline
            , B.color (rgb255 2 2 2)
            ]
                ++ selectedStyles
    in
    row [ width fill, paddingXY 0 10 ]
        [ el [ width (px 200) ] (text "Difficulty")
        , button
            (styles Easy
                ++ [ B.roundEach
                        { topLeft = 4
                        , bottomLeft = 4
                        , topRight = 0
                        , bottomRight = 0
                        }
                   ]
            )
            { label = text "Easy"
            , onPress = Just (SetDifficulty Easy)
            }
        , button (styles Medium)
            { label = text "Medium"
            , onPress = Just (SetDifficulty Medium)
            }
        , button
            (styles Hard
                ++ [ B.roundEach
                        { topLeft = 0
                        , bottomLeft = 0
                        , topRight = 4
                        , bottomRight = 4
                        }
                   ]
            )
            { label = text "Hard"
            , onPress = Just (SetDifficulty Hard)
            }
        ]


validSetup : { model | gridSize : Int, minGridSize : Int, maxGridSize : Int } -> Bool
validSetup { gridSize, minGridSize, maxGridSize } =
    gridSize >= minGridSize && gridSize <= maxGridSize


disabledButtonStyles : List (Element.Attribute msg)
disabledButtonStyles =
    [ Bg.color (rgb255 220 220 220)
    , F.color (rgb255 150 150 150)
    , Element.mouseOver
        []
    ]


viewStartMenu : Model -> Element Msg
viewStartMenu model =
    column [ width (px 1024), centerX, padding 16 ]
        [ el
            [ centerX
            , heavy
            , paddingEach { top = 16, left = 0, bottom = 32, right = 0 }
            , F.size (round (scaled 4))
            ]
            (text "Welcome to Elmsweeper")
        , column [ width fill ]
            [ I.text []
                { onChange = maybeSet SetGridSize
                , text =
                    if model.gridSize > 0 then
                        String.fromInt model.gridSize

                    else
                        ""
                , placeholder = Nothing
                , label = I.labelLeft [ centerY, width (px 195), F.alignLeft ] (text "Size")
                }
            , viewDifficultySelect model.difficulty
            ]
        , el [ centerX, padding 8, F.color (rgb255 255 0 0) ]
            (if validSetup model then
                text ""

             else
                text ("Size must be between " ++ String.fromInt model.minGridSize ++ " and " ++ String.fromInt model.maxGridSize)
            )
        , button
            (buttonStyles
                ++ [ centerX, width fill ]
                ++ (if validSetup model then
                        []

                    else
                        disabledButtonStyles
                   )
            )
            { label = text "Start game"
            , onPress =
                if validSetup model then
                    Just StartGame

                else
                    Nothing
            }
        ]


coordsToString : Coordinates -> String
coordsToString ( x, y ) =
    String.fromInt x ++ "x" ++ String.fromInt y


viewCell : Int -> ( Coordinates, Cell ) -> Svg.Svg Msg
viewCell cellSize ( ( y, x ), cell ) =
    let
        yPos =
            y * cellSize

        xPos =
            x * cellSize

        xPosStr =
            String.fromInt xPos

        yPosStr =
            String.fromInt yPos

        cellSizeStr =
            cellSize |> String.fromInt

        padding =
            floor (toFloat cellSize * 0.1)

        baseAttrs =
            [ SVGA.x xPosStr
            , SVGA.y yPosStr
            , SVGA.width cellSizeStr
            , SVGA.height cellSizeStr
            , SVGA.stroke "black"
            , SVGA.fill "gray"
            , SVGA.cursor "pointer"
            ]
    in
    case cell of
        Cell Hidden _ ->
            Svg.rect ([ SVGE.onClick (CellClick ( y, x )), onRightClick (CellRightClick ( y, x )) ] ++ baseAttrs) []

        Cell Visible Bomb ->
            Svg.g []
                [ Svg.rect (baseAttrs ++ [ SVGA.fill "lightgray" ]) []
                , Svg.image
                    (baseAttrs
                        ++ [ SVGA.xlinkHref "bomb.svg"
                           , SVGA.width (String.fromInt (floor (toFloat cellSize * 0.8)))
                           , SVGA.x (xPos + padding |> String.fromInt)
                           ]
                    )
                    []
                ]

        Cell Visible (AdjacentBombs bombCount) ->
            if bombCount > 0 then
                Svg.g [ SVGE.onClick (VisibleCellClick ( y, x )) ]
                    [ Svg.rect
                        (baseAttrs ++ [ SVGA.fill "lightgray" ])
                        []
                    , Svg.text_
                        (baseAttrs
                            ++ [ SVGA.fill (numToColor bombCount)
                               , SVGA.fontSize (String.fromInt (cellSize // 2))
                               , SVGA.x (String.fromInt (xPos + (cellSize // 2)))
                               , SVGA.y (String.fromInt (yPos + ((cellSize // 4) * 3)))
                               , SVGA.textAnchor "middle"
                               , SVGA.stroke "none"
                               ]
                        )
                        [ Svg.text (bombCount |> String.fromInt) ]
                    ]

            else
                Svg.rect (baseAttrs ++ [ SVGA.fill "lightgray" ]) []

        Cell Flag _ ->
            Svg.g [ onRightClick (CellRightClick ( y, x )) ]
                [ Svg.rect baseAttrs []
                , Svg.image
                    (baseAttrs
                        ++ [ SVGA.xlinkHref "flag.svg"
                           , SVGA.width (String.fromInt (floor (toFloat cellSize * 0.8)))
                           , SVGA.x (xPos + padding |> String.fromInt)
                           ]
                    )
                    []
                ]


viewGrid : Model -> Element Msg
viewGrid model =
    let
        gridList =
            Dict.toList model.grid

        canvasSize =
            1024

        canvasSizeStr =
            canvasSize |> String.fromInt
    in
    el [ width (px canvasSize), noTextSelect ]
        (Element.html
            (Svg.svg
                [ SVGA.width canvasSizeStr
                , SVGA.height canvasSizeStr
                , SVGA.viewBox ("0 0 " ++ canvasSizeStr ++ " " ++ canvasSizeStr)
                ]
                (List.map (viewCell (canvasSize // model.gridSize)) gridList)
            )
        )


gameStatusToEmoji : GameStatus -> String
gameStatusToEmoji gameStatus =
    case gameStatus of
        Running ->
            "happy.svg"

        Won ->
            "very-happy.svg"

        Lost ->
            "dead.svg"


gameStatusToGreeting : GameStatus -> String
gameStatusToGreeting gameStatus =
    case gameStatus of
        Running ->
            "Game is running"

        Won ->
            "You won! Congratulations"

        Lost ->
            "You lost! Better luck next time"


zeroPad : Int -> String
zeroPad =
    String.fromInt
        >> String.padLeft 2 '0'


viewCounter : String -> Int -> Element msg
viewCounter title durationSeconds =
    let
        hours =
            durationSeconds // (60 * 60)

        minutes =
            durationSeconds // 60

        seconds =
            if durationSeconds > 60 then
                remainderBy 60 durationSeconds

            else
                durationSeconds
    in
    el []
        (text
            ([ hours, minutes, seconds ]
                |> List.map zeroPad
                |> List.intersperse ":"
                |> (::) title
                |> String.concat
            )
        )


scaled =
    Element.modular 16 1.25


noOutline =
    Element.htmlAttribute <| HA.style "box-shadow" "none"


noTextSelect =
    Element.htmlAttribute <| HA.style "user-select" "none"


buttonStyles : List (Element.Attribute msg)
buttonStyles =
    [ Bg.color (rgb255 18 147 216)
    , padding 16
    , B.rounded 3
    , F.color (rgb255 255 255 255)
    , bold
    , Element.mouseOver
        [ Bg.color (rgb255 68 197 255) ]
    , noOutline
    , F.center
    ]


viewInGame : Model -> Element Msg
viewInGame model =
    el [ width (px 1024), centerX, padding 16 ]
        (column [ centerX ]
            [ el [ centerX, heavy, F.size (round (scaled 3)) ] (text (gameStatusToGreeting model.gameStatus))
            , row [ paddingXY 0 16, spaceEvenly, width fill ]
                [ button buttonStyles { label = text "Back to menu", onPress = Just BackToMenu }
                , button buttonStyles { label = text "Reset Game", onPress = Just ResetGame }
                ]
            , row [ paddingXY 0 8, spaceEvenly, width fill ]
                [ text ("Flags remaining: " ++ String.fromInt model.remainingFlags)
                , image [ width (px 80), height (px 80) ] { src = gameStatusToEmoji model.gameStatus, description = "smiley" }
                , viewCounter "Timer: " model.gameDurationSeconds
                ]
            , viewGrid model
            ]
        )


view : Model -> Html.Html Msg
view model =
    layout [] <|
        case model.gameState of
            StartMenu ->
                viewStartMenu model

            InGame ->
                viewInGame model


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        hasStarted =
            not <| isFirstAction model.grid
    in
    case ( hasStarted, model.gameStatus ) of
        -- Lost, stop counting
        ( _, Lost ) ->
            Sub.none

        -- Won, stop counting
        ( _, Won ) ->
            Sub.none

        -- Started and not lost
        ( True, _ ) ->
            every 1000 SecondElapsed

        -- else, don't count
        ( _, _ ) ->
            Sub.none



---- PROGRAM ----


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
