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
        [ onKeyDown (Decode.map Keypress directionKeyDecoder)
        , onKeyDown (Decode.map UpdateStage updateCommandDecoder)
        ]


type alias Model =
    { currentLevelIndex : Int
    , selectedStage : Sokoban.StageInfo
    , currentState : Sokoban.StageInfo
    }


initialModel : () -> ( Model, Cmd msg )
initialModel _ =
    let
        initialStage =
            SokobanJson.initialStage
    in
    ( { currentLevelIndex = 0
      , selectedStage = initialStage
      , currentState = { initialStage | boxCount = 0 }
      }
    , Cmd.none
    )


directionKeyDecoder : Decode.Decoder Sokoban.Direction
directionKeyDecoder =
    Decode.map Sokoban.toDirection (Decode.field "key" Decode.string)


type StageUpdateCommand
    = Undo
    | Restart
    | NextStage
    | Nothing


stringToUpdateCommand : String -> StageUpdateCommand
stringToUpdateCommand key =
    case key of
        "Backspace" ->
            Undo

        "Escape" ->
            Restart

        "Enter" ->
            NextStage

        _ ->
            Nothing


updateCommandDecoder : Decode.Decoder StageUpdateCommand
updateCommandDecoder =
    Decode.map stringToUpdateCommand (Decode.field "key" Decode.string)


type Msg
    = Keypress Sokoban.Direction
    | UpdateStage StageUpdateCommand


handleRestart : Model -> Model
handleRestart model =
    let
        selectedStage =
            model.selectedStage
    in
    { model | currentState = { selectedStage | boxCount = 0 } }


handleGoToNextStage : Sokoban.StageInfo -> Model -> Model
handleGoToNextStage nextStage model =
    let
        isFinished =
            model.currentState.boxCount == model.selectedStage.boxCount
    in
    if not isFinished then
        model

    else
        { model
            | currentLevelIndex = model.currentLevelIndex + 1
            , selectedStage = nextStage
            , currentState = { nextStage | boxCount = 0 }
        }


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

        UpdateStage command ->
            case command of
                Undo ->
                    ( model, Cmd.none )

                Restart ->
                    ( model |> handleRestart, Cmd.none )

                NextStage ->
                    let
                        maybeNextStage =
                            SokobanJson.getStageByIndex (model.currentLevelIndex + 1)
                    in
                    case maybeNextStage of
                        Just nextStage ->
                            ( model |> handleGoToNextStage nextStage, Cmd.none )

                        Maybe.Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )


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

        -- TODO:
        -- , tr []
        --     [ td [ class "command" ] [ text "Backspace" ]
        --     , td [ class "description" ] [ text "Undo" ]
        --     ]
        , tr []
            [ td [ class "command" ] [ text "Escape" ]
            , td [ class "description" ] [ text "Restart level" ]
            ]
        ]


view : Model -> Html Msg
view model =
    div [ class "game" ]
        [ div [ class "sokoban-level" ] [ text ("Level " ++ (model.currentLevelIndex + 1 |> String.fromInt)) ]
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
