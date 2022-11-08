module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (class)
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map Keypress keyDecoder)
        ]


type alias Position =
    { x : Int
    , y : Int
    }


initialModel : () -> ( Model, Cmd msg )
initialModel _ =
    ( { playerPosition = { x = 1, y = 1 }
      , stage = initialStage
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


type alias Model =
    { playerPosition : Position
    , stage : List String
    }


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other


keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


type Msg
    = Keypress Direction


updatePosition : Direction -> Position -> Position
updatePosition direction position =
    case direction of
        Up ->
            { position | y = position.y - 1 }

        Down ->
            { position | y = position.y + 1 }

        Right ->
            { position | x = position.x + 1 }

        Left ->
            { position | x = position.x - 1 }

        _ ->
            position


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Keypress direction ->
            ( { model | playerPosition = updatePosition direction model.playerPosition }, Cmd.none )


type Cell
    = Player
    | Wall
    | Box
    | Objective
    | Empty


stringToCell : String -> Cell
stringToCell str =
    if str == "@" then
        Player

    else if str == "#" then
        Wall

    else if str == "$" then
        Box

    else if str == "." then
        Objective

    else
        Empty


getCellClass : Cell -> String
getCellClass cell =
    case cell of
        -- プレイヤーは位置をもとにクラスをつける
        Player ->
            ""

        Wall ->
            "wall"

        Box ->
            "box"

        Objective ->
            "objective"

        Empty ->
            "empty"


cn : List String -> String
cn classNames =
    String.join " " classNames


stageCell : Position -> Int -> Int -> String -> Html msg
stageCell playerPosition rowNumber colNumber cellString =
    let
        cell =
            cellString |> stringToCell

        cellClass =
            if playerPosition == { y = rowNumber, x = colNumber } then
                "player"

            else
                getCellClass cell
    in
    div [ class (cn [ "cell", cellClass ]) ] []


stageLine : Position -> Int -> String -> Html msg
stageLine playerPosition rowNumber line =
    div [ class "board-row" ] (List.indexedMap (stageCell playerPosition rowNumber) (String.split "" line))


view : Model -> Html Msg
view model =
    div []
        (List.indexedMap (stageLine model.playerPosition) model.stage)
