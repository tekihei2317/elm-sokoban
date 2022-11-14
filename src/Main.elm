module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as Decode
import Sokoban


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


type alias Model =
    { stage : Sokoban.Stage
    , playerPosition : Position
    , totalBoxCount : Int
    , openedBoxCount : Int
    }


initialModel : () -> ( Model, Cmd msg )
initialModel _ =
    ( { stage = initialStage |> Sokoban.convertStringStage
      , playerPosition = { y = 1, x = 1 }
      , totalBoxCount = 2
      , openedBoxCount = 0
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
    case msg of
        Keypress direction ->
            let
                { stage, isPlayerMoved, openedBoxCountDiff } =
                    Sokoban.updateStage model.stage direction model.playerPosition

                playerPosition =
                    if isPlayerMoved then
                        Sokoban.updatePosition direction model.playerPosition

                    else
                        model.playerPosition
            in
            ( { model
                | stage = stage
                , playerPosition = playerPosition
                , openedBoxCount = model.openedBoxCount + openedBoxCountDiff
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


view : Model -> Html Msg
view model =
    div []
        [ div [] (model.stage |> Array.toList |> List.indexedMap stageLine)
        , div [ class "clear-title" ]
            [ text
                (if model.totalBoxCount == model.openedBoxCount then
                    "Game Clear!"

                 else
                    ""
                )
            ]
        ]
