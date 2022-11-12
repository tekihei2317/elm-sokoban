module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events
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


type alias Model =
    { stage : Array (Array Cell)
    , line : Array Int
    , playerPosition : Position
    }


initialModel : () -> ( Model, Cmd msg )
initialModel _ =
    ( { stage = initialStage |> convertStringStage
      , line = Array.initialize 5 identity
      , playerPosition = { y = 1, x = 1 }
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


convertStringStage : List String -> Array (Array Cell)
convertStringStage stage =
    stage |> Array.fromList |> Array.map (\line -> line |> String.split "" |> List.map stringToCell |> Array.fromList)


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
    | Increment


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


type alias Neighborhood =
    { current : Maybe Cell
    , next : Maybe Cell
    , afterNext : Maybe Cell
    }


updateNeighborhood : Neighborhood -> Neighborhood
updateNeighborhood cells =
    cells


updateStage : Array (Array Cell) -> Direction -> Position -> Array (Array Cell)
updateStage stage direction playerPosition =
    -- 現在のマス、次のマス、その次のマスを取得する
    -- 更新のロジックを書く（判定処理）
    -- ステージを更新する
    let
        currentCell =
            Empty

        maybeLine =
            Array.get 1 stage
    in
    case maybeLine of
        Nothing ->
            stage

        Just line ->
            stage |> Array.set 1 (line |> Array.set 1 Empty)


incrementLine : Array Int -> Array Int
incrementLine line =
    line |> Array.set 0 3 |> Array.set 1 1 |> Array.set 2 4 |> Array.set 3 1 |> Array.set 4 5


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Keypress direction ->
            ( { model
                | stage = updateStage model.stage direction model.playerPosition
                , playerPosition = updatePosition direction model.playerPosition
              }
            , Cmd.none
            )

        Increment ->
            ( { model | line = incrementLine model.line }, Cmd.none )


type alias OnObjective =
    Bool


type Cell
    = Empty
    | Objective
    | Wall
    | Player OnObjective
    | Box OnObjective


stringToCell : String -> Cell
stringToCell str =
    if str == "@" then
        Player False

    else if str == "#" then
        Wall

    else if str == "$" then
        Box False

    else if str == "." then
        Objective

    else
        Empty


getCellClass : Cell -> String
getCellClass cell =
    case cell of
        -- プレイヤーは位置をもとにクラスをつける
        Player _ ->
            "player"

        Wall ->
            "wall"

        Box _ ->
            "box"

        Objective ->
            "objective"

        Empty ->
            "empty"


cn : List String -> String
cn classNames =
    String.join " " classNames


stageCell : Int -> Int -> Cell -> Html msg
stageCell rowNumber colNumber cell =
    let
        cellClass =
            getCellClass cell
    in
    div [ class (cn [ "cell", cellClass ]) ] []


stageLine : Int -> Array Cell -> Html msg
stageLine rowNumber line =
    div [ class "board-row" ] (line |> Array.toList |> List.indexedMap (stageCell rowNumber))


view : Model -> Html Msg
view model =
    div []
        [ div [] (model.stage |> Array.toList |> List.indexedMap stageLine)
        , div [] (model.line |> Array.toList |> List.map (\number -> span [] [ text (String.fromInt number) ]))
        , button [ Html.Events.onClick Increment ] [ text "Increment" ]
        ]
