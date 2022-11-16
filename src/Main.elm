module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Sokoban
import SokobanJson


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map Keypress keyDecoder)
        ]


type alias Position =
    { x : Int
    , y : Int
    }


type alias StageInfo =
    { stage : Sokoban.Stage
    , playerPosition : Position
    , boxCount : Int
    }


type alias Model =
    { currentLevelIndex : Int
    , selectedStage : StageInfo
    , currentState : StageInfo
    }


initialModel : () -> ( Model, Cmd msg )
initialModel _ =
    ( { currentLevelIndex = 0
      , selectedStage = { stage = initialStage |> Sokoban.convertStringStage, playerPosition = { y = 1, x = 1 }, boxCount = 2 }
      , currentState = { stage = initialStage |> Sokoban.convertStringStage, playerPosition = { y = 1, x = 1 }, boxCount = 0 }
      }
    , Cmd.none
    )


initialStage : List String
initialStage =
    [ "#####"
    , "#@  #"
    , "# $ #"
    , "### #"
    , "#.$ #"
    , "#  .#"
    , "#####"
    ]


keyDecoder =
    Decode.map Sokoban.toDirection (Decode.field "key" Decode.string)


type Msg
    = Keypress Sokoban.Direction


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    let
        currentState =
            model.currentState
    in
    case msg of
        Keypress direction ->
            let
                { stage, isPlayerMoved, openedBoxCountDiff } =
                    Sokoban.updateStage currentState.stage direction currentState.playerPosition

                playerPosition =
                    if isPlayerMoved then
                        Sokoban.updatePosition direction currentState.playerPosition

                    else
                        currentState.playerPosition
            in
            ( { model
                | currentState =
                    { currentState
                        | stage = stage
                        , playerPosition = playerPosition
                        , boxCount = currentState.boxCount + openedBoxCountDiff
                    }
              }
            , Cmd.none
            )


cn : List String -> String
cn classNames =
    String.join " " classNames


stageCell : Int -> Int -> Sokoban.Cell -> Html msg
stageCell _ _ cell =
    let
        cellClass =
            Sokoban.getCellClass cell
    in
    div [ class (cn [ "cell", cellClass ]) ] []


stageLine : Int -> Array Sokoban.Cell -> Html msg
stageLine rowNumber line =
    div [ class "board-row" ] (line |> Array.toList |> List.indexedMap (stageCell rowNumber))


helpView : Html msg
helpView =
    table [ class "help" ]
        [ tr []
            [ td [ class "command" ] [ text "↑" ]
            , td [ class "description" ] [ text "Move" ]
            ]
        , tr []
            [ td [ class "command" ] [ text "← →" ]
            , td [ class "description" ] [ text "Move" ]
            ]
        , tr []
            [ td [ class "command" ] [ text "↓" ]
            , td [ class "description" ] [ text "Move" ]
            ]
        , tr []
            [ td [ class "command" ] [ text "Backspace" ]
            , td [ class "description" ] [ text "Undo" ]
            ]
        , tr []
            [ td [ class "command" ] [ text "Escape" ]
            , td [ class "description" ] [ text "Restart level" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ div [ class "sokoban-level" ] [ text "Level 1" ]
        , div [] (model.currentState.stage |> Array.toList |> List.indexedMap stageLine)
        , helpView
        , div [ class "sokoban-state" ]
            (if model.currentState.boxCount < model.selectedStage.boxCount then
                []

             else
                [ div [ class "description" ]
                    [ text "LEVEL completed" ]
                , div [ class "action" ]
                    [ text "Press ENTER to load next LEVEL" ]
                ]
            )
        ]
