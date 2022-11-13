module Main exposing (main)

import Array exposing (Array)
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
subscriptions _ =
    Sub.batch
        [ onKeyDown (Decode.map Keypress keyDecoder)
        ]


type alias Position =
    { x : Int
    , y : Int
    }


type alias Model =
    { stage : Array (Array Cell)
    , playerPosition : Position
    , totalBoxCount : Int
    , openedBoxCount : Int
    }


initialModel : () -> ( Model, Cmd msg )
initialModel _ =
    ( { stage = initialStage |> convertStringStage
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


wrapCellsWithJust : Cell -> Cell -> Cell -> Neighborhood
wrapCellsWithJust cell nextCell afterNextCell =
    { current = Just cell
    , next = Just nextCell
    , afterNext = Just afterNextCell
    }


type alias UpdateNeighborhoodResult =
    { neighborhood : Neighborhood
    , isPlayerMoved : Bool
    , openedBoxCountDiff : Int
    }


updateJustNeighborhood : Cell -> Cell -> Cell -> UpdateNeighborhoodResult
updateJustNeighborhood cell nextCell afterNextCell =
    let
        noChange =
            { neighborhood = wrapCellsWithJust cell nextCell afterNextCell
            , isPlayerMoved = False
            , openedBoxCountDiff = 0
            }

        cellAfterPlayerMoved isPlayerOnObjective =
            if isPlayerOnObjective then
                Objective

            else
                Empty
    in
    case cell of
        Player onObjective ->
            case nextCell of
                -- 隣のマスが空マスの場合
                Empty ->
                    { neighborhood =
                        wrapCellsWithJust
                            (cellAfterPlayerMoved onObjective)
                            (Player False)
                            afterNextCell
                    , isPlayerMoved = True
                    , openedBoxCountDiff = 0
                    }

                -- 隣のマスがゴールの場合
                Objective ->
                    { neighborhood =
                        wrapCellsWithJust
                            (cellAfterPlayerMoved onObjective)
                            (Player True)
                            afterNextCell
                    , isPlayerMoved = True
                    , openedBoxCountDiff = 0
                    }

                -- 隣のマスが宝箱の場合
                Box isNextOnObject ->
                    case afterNextCell of
                        -- 隣の隣のマスが空の場合
                        Empty ->
                            { neighborhood =
                                wrapCellsWithJust
                                    (cellAfterPlayerMoved onObjective)
                                    (Player isNextOnObject)
                                    (Box False)
                            , isPlayerMoved = True
                            , openedBoxCountDiff =
                                if isNextOnObject then
                                    -1

                                else
                                    0
                            }

                        -- 隣の隣のマスがゴールの場合
                        Objective ->
                            { neighborhood =
                                wrapCellsWithJust
                                    (cellAfterPlayerMoved onObjective)
                                    (Player isNextOnObject)
                                    (Box True)
                            , isPlayerMoved = True
                            , openedBoxCountDiff =
                                if isNextOnObject then
                                    0

                                else
                                    1
                            }

                        _ ->
                            noChange

                _ ->
                    noChange

        _ ->
            noChange


updateNeighborhood : Neighborhood -> UpdateNeighborhoodResult
updateNeighborhood cells =
    let
        noChange =
            { neighborhood = cells
            , isPlayerMoved = False
            , openedBoxCountDiff = 0
            }
    in
    -- Maybeがつらいだろうなぁ
    case cells.current of
        Nothing ->
            noChange

        Just cell ->
            case cells.next of
                Nothing ->
                    noChange

                Just nextCell ->
                    case cells.afterNext of
                        Nothing ->
                            noChange

                        Just afterNextCell ->
                            updateJustNeighborhood cell nextCell afterNextCell


getNextPosition : Direction -> Position -> Position
getNextPosition direction position =
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


getNeighborhood : Array (Array Cell) -> Direction -> Position -> Neighborhood
getNeighborhood stage direction position =
    let
        nextPosition =
            position |> getNextPosition direction

        afterNextPosition =
            nextPosition |> getNextPosition direction
    in
    { current = stage |> Array.get position.y |> Maybe.andThen (Array.get position.x)
    , next = stage |> Array.get nextPosition.y |> Maybe.andThen (Array.get nextPosition.x)
    , afterNext = stage |> Array.get afterNextPosition.y |> Maybe.andThen (Array.get afterNextPosition.x)
    }


type alias Stage =
    Array (Array Cell)


updateCell : Position -> Maybe Cell -> Stage -> Stage
updateCell position maybeCell stage =
    let
        maybeRow =
            stage |> Array.get position.y
    in
    -- つらい
    case maybeCell of
        Nothing ->
            stage

        Just cell ->
            case maybeRow of
                Nothing ->
                    stage

                Just row ->
                    stage |> Array.set position.y (row |> Array.set position.x cell)


type alias UpdateStageResult =
    { stage : Stage
    , isPlayerMoved : Bool
    , openedBoxCountDiff : Int
    }


updateStage : Stage -> Direction -> Position -> UpdateStageResult
updateStage stage direction playerPosition =
    let
        { neighborhood, isPlayerMoved, openedBoxCountDiff } =
            getNeighborhood stage direction playerPosition |> updateNeighborhood

        nextPosition =
            playerPosition |> getNextPosition direction

        afterNextPosition =
            nextPosition |> getNextPosition direction
    in
    { stage =
        stage
            |> updateCell playerPosition neighborhood.current
            |> updateCell nextPosition neighborhood.next
            |> updateCell afterNextPosition neighborhood.afterNext
    , isPlayerMoved = isPlayerMoved
    , openedBoxCountDiff = openedBoxCountDiff
    }


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Keypress direction ->
            let
                { stage, isPlayerMoved, openedBoxCountDiff } =
                    updateStage model.stage direction model.playerPosition

                playerPosition =
                    if isPlayerMoved then
                        updatePosition direction model.playerPosition

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

        Box onObject ->
            cn
                [ "box"
                , if onObject then
                    "opened"

                  else
                    ""
                ]

        Objective ->
            "objective"

        Empty ->
            "empty"


cn : List String -> String
cn classNames =
    String.join " " classNames


stageCell : Int -> Int -> Cell -> Html msg
stageCell _ _ cell =
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
        , div [ class "clear-title" ]
            [ text
                (if model.totalBoxCount == model.openedBoxCount then
                    "Game Clear!"

                 else
                    ""
                )
            ]
        ]
