module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class)


main : Program () Model Msg
main =
    Browser.sandbox { init = initialModel, update = update, view = view }


initialModel : Model
initialModel =
    { counter = 0
    , stage = initialStage
    }


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
    { counter : Int
    , stage : List String
    }


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | counter = model.counter + 1 }

        Decrement ->
            { model | counter = model.counter - 1 }


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


cellClass : Cell -> String
cellClass cell =
    case cell of
        Player ->
            "player"

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


stageCell : String -> Html msg
stageCell cellString =
    let
        cell =
            cellString |> stringToCell
    in
    div [ class (cn [ "cell", cellClass cell ]) ] []


stageLine : String -> Html msg
stageLine line =
    div [ class "board-row" ] (List.map stageCell (String.split "" line))


view : Model -> Html Msg
view model =
    div []
        (List.map stageLine model.stage)
