module Main exposing (..)

import Browser
import Dict exposing (Dict)
import Grid
import Html exposing (Html, b, button, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Random exposing (Generator, generate)
import RightClick exposing (onRightClick)
import Types exposing (..)


---- MODEL ----


type GameState
    = StartMenu
    | InGame


type alias Model =
    { randomSeed : Random.Seed
    , gameState : GameState
    , grid : Grid
    , gameStatus : GameStatus
    , bombAmount : Int
    , remainingFlags : Int
    }


init : Int -> ( Model, Cmd Msg )
init initialSeed =
    let
        seed =
            Random.initialSeed initialSeed

        bombAmount =
            40
    in
    ( { randomSeed = seed
      , gameState = StartMenu
      , grid = Grid.generate 20 bombAmount seed
      , gameStatus = Running
      , bombAmount = bombAmount
      , remainingFlags = bombAmount
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        StartGame ->
            ( { model | gameState = InGame }, Cmd.none )

        ResetGame ->
            let
                newSeed =
                    Random.step (Random.int 0 512) model.randomSeed
                        |> Tuple.second

                newModel =
                    updateGridAndStatus (\_ -> Grid.generate 20 model.bombAmount newSeed) model
            in
            ( { newModel
                | randomSeed = newSeed
                , remainingFlags = newModel.bombAmount
              }
            , Cmd.none
            )

        CellClick ( x, y ) ->
            ( updateGridAndStatus (Grid.visit ( x, y )) model
            , Cmd.none
            )

        VisibleCellClick ( x, y ) ->
            ( updateGridAndStatus (Grid.showSurrounding ( x, y )) model
            , Cmd.none
            )

        CellRightClick ( x, y ) ->
            let
                m =
                    updateGridAndStatus (Grid.flag ( x, y )) model
            in
            ( { m | remainingFlags = m.bombAmount - Grid.flagsPlaced m.grid }
            , Cmd.none
            )

        BackToMenu ->
            update ResetGame { model | gameState = StartMenu }


updateGridAndStatus : (Grid -> Grid) -> Model -> Model
updateGridAndStatus gridUpdater model =
    let
        newGrid =
            gridUpdater model.grid
    in
    { model | grid = newGrid, gameStatus = Grid.evaluateGameStatus newGrid }



---- VIEW ----


numToString : Int -> String
numToString n =
    case n of
        1 ->
            "one"

        2 ->
            "two"

        3 ->
            "three"

        4 ->
            "four"

        5 ->
            "five"

        6 ->
            "six"

        7 ->
            "seven"

        8 ->
            "eight"

        _ ->
            ""


viewStartMenu : Model -> Html Msg
viewStartMenu model =
    div []
        [ h1 [] [ text "Welcome to Elmsweeper" ]
        , button [ onClick StartGame ] [ text "New game" ]
        ]


viewCell : ( Coordinates, Cell ) -> Html Msg
viewCell ( coords, cell ) =
    case cell of
        Cell Hidden _ ->
            div
                [ class "hidden-cell"
                , onClick (CellClick coords)
                , onRightClick (CellRightClick coords)
                ]
                []

        Cell Visible Bomb ->
            div [ class "visible-cell bomb" ]
                []

        Cell Visible (AdjacentBombs bombCount) ->
            div [ class ("visible-cell " ++ numToString bombCount), onClick (VisibleCellClick coords) ]
                [ b []
                    [ text <|
                        if bombCount > 0 then
                            String.fromInt bombCount
                        else
                            ""
                    ]
                ]

        Cell Flag _ ->
            div [ class "visible-cell flag", onRightClick (CellRightClick coords) ] []


viewGrid grid =
    let
        gridList =
            Dict.toList grid
    in
    div [ class "gameBoard" ] <| List.map viewCell gridList


gameStatusToString : GameStatus -> String
gameStatusToString gameStatus =
    case gameStatus of
        Running ->
            "running"

        Won ->
            "won"

        Lost ->
            "lost"


gameStatusToGreeting : GameStatus -> String
gameStatusToGreeting gameStatus =
    case gameStatus of
        Running ->
            "Game is running"

        Won ->
            "You won! Congratulations"

        Lost ->
            "You lost! Better luck next time"


viewInGame : Model -> Html Msg
viewInGame model =
    div []
        [ div []
            [ h1 [] [ text (gameStatusToGreeting model.gameStatus) ]
            , div [ class ("gamestatus " ++ gameStatusToString model.gameStatus) ] []
            , ul [ class "button-row" ]
                [ li [] [ button [ onClick BackToMenu ] [ text "Back to menu" ] ]
                , li [] [ button [ onClick ResetGame ] [ text "Reset Game" ] ]
                ]
            , div [] [ text ("Flags remaining: " ++ String.fromInt model.remainingFlags) ]
            , viewGrid model.grid
            ]
        ]


view : Model -> Html Msg
view model =
    case model.gameState of
        StartMenu ->
            viewStartMenu model

        InGame ->
            viewInGame model



---- PROGRAM ----


main : Program Int Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
