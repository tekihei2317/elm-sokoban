module Sokoban exposing (Cell, Direction, Position, Stage, StageInfo, convertStringStage, getCellClass, toDirection, updatePosition, updateStage)

import Array exposing (Array)


type alias OnObjective =
    Bool


type Cell
    = Empty
    | Objective
    | Wall
    | Player OnObjective
    | Box OnObjective


type alias Stage =
    Array (Array Cell)


type alias StageInfo =
    { stage : Stage
    , playerPosition : Position
    , boxCount : Int
    }


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


type alias Position =
    { x : Int
    , y : Int
    }


type alias UpdateStageResult =
    { stage : Stage
    , isPlayerMoved : Bool
    , openedBoxCountDiff : Int
    }


type alias Neighborhood =
    { current : Maybe Cell
    , next : Maybe Cell
    , afterNext : Maybe Cell
    }


type alias UpdateNeighborhoodResult =
    { neighborhood : Neighborhood
    , isPlayerMoved : Bool
    , openedBoxCountDiff : Int
    }


cn : List String -> String
cn classNames =
    String.join " " classNames


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


foldPlayers : Array (Maybe Position) -> Maybe Position
foldPlayers maybePlayers =
    maybePlayers
        |> Array.foldl
            (\maybePlayer sum ->
                case ( sum, maybePlayer ) of
                    ( Just player, _ ) ->
                        Just player

                    ( Nothing, Just player ) ->
                        Just player

                    ( Nothing, Nothing ) ->
                        Nothing
            )
            Nothing


findPlayerPosition : Stage -> Position
findPlayerPosition stage =
    stage
        |> Array.indexedMap
            (\y line ->
                line
                    |> Array.indexedMap
                        (\x cell ->
                            case cell of
                                Player _ ->
                                    Just { y = y, x = x }

                                _ ->
                                    Nothing
                        )
                    |> foldPlayers
            )
        |> foldPlayers
        |> Maybe.withDefault { y = 1, x = 1 }


countBoxes : Stage -> Int
countBoxes stage =
    stage
        |> Array.map
            (\line ->
                line
                    |> Array.foldl
                        (\cell count ->
                            case cell of
                                Box _ ->
                                    count + 1

                                _ ->
                                    count
                        )
                        0
            )
        |> Array.foldl (+) 0


convertStringStage : Array String -> StageInfo
convertStringStage stringStage =
    let
        stage =
            stringStage |> Array.map (\line -> line |> String.split "" |> List.map stringToCell |> Array.fromList)
    in
    { stage = stage
    , playerPosition = findPlayerPosition stage
    , boxCount = countBoxes stage
    }


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


wrapCellsWithJust : Cell -> Cell -> Cell -> Neighborhood
wrapCellsWithJust cell nextCell afterNextCell =
    { current = Just cell
    , next = Just nextCell
    , afterNext = Just afterNextCell
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
    case ( cell, nextCell, afterNextCell ) of
        -- 隣のマスが空マスの場合
        ( Player onObjective, Empty, _ ) ->
            { neighborhood =
                wrapCellsWithJust
                    (cellAfterPlayerMoved onObjective)
                    (Player False)
                    afterNextCell
            , isPlayerMoved = True
            , openedBoxCountDiff = 0
            }

        -- 隣のマスがゴールの場合
        ( Player onObjective, Objective, _ ) ->
            { neighborhood =
                wrapCellsWithJust
                    (cellAfterPlayerMoved onObjective)
                    (Player True)
                    afterNextCell
            , isPlayerMoved = True
            , openedBoxCountDiff = 0
            }

        -- 隣のマスが宝箱で、その隣が空マスの場合
        ( Player onObjective, Box isNextOnObjective, Empty ) ->
            { neighborhood =
                wrapCellsWithJust
                    (cellAfterPlayerMoved onObjective)
                    (Player isNextOnObjective)
                    (Box False)
            , isPlayerMoved = True
            , openedBoxCountDiff =
                if isNextOnObjective then
                    -1

                else
                    0
            }

        -- 隣のマスが宝箱で、その隣がゴールの場合
        ( Player onObjective, Box isNextOnObjective, Objective ) ->
            { neighborhood =
                wrapCellsWithJust
                    (cellAfterPlayerMoved onObjective)
                    (Player isNextOnObjective)
                    (Box True)
            , isPlayerMoved = True
            , openedBoxCountDiff =
                if isNextOnObjective then
                    0

                else
                    1
            }

        ( _, _, _ ) ->
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
    Maybe.map3 updateJustNeighborhood cells.current cells.next cells.afterNext
        |> Maybe.withDefault noChange


updateCell : Position -> Maybe Cell -> Stage -> Stage
updateCell position maybeCell stage =
    let
        maybeRow =
            stage |> Array.get position.y
    in
    -- n次元配列のいい感じの更新方法が思いつきませんでした...
    case maybeCell of
        Nothing ->
            stage

        Just cell ->
            case maybeRow of
                Nothing ->
                    stage

                Just row ->
                    stage |> Array.set position.y (row |> Array.set position.x cell)


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
    -- ここ、CellのMaybeを外していい感じにやりたいです
    { stage =
        stage
            |> updateCell playerPosition neighborhood.current
            |> updateCell nextPosition neighborhood.next
            |> updateCell afterNextPosition neighborhood.afterNext
    , isPlayerMoved = isPlayerMoved
    , openedBoxCountDiff = openedBoxCountDiff
    }
