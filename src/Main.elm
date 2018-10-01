module Main exposing (Msg(..), main, update, view)

import Browser exposing (Document, document)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import TypedSvg exposing (rect, svg)
import TypedSvg.Attributes exposing (fill, height, stroke, strokeWidth, viewBox, width, x, y)
import TypedSvg.Types exposing (Fill(..), px)



-- MODEL


type Direction
    = North
    | East
    | South
    | West


type Msg
    = Place
    | Move
    | TurnLeft
    | TurnRight
    | InputX String
    | InputY String


type alias Coordinate =
    { x : Int
    , y : Int
    }


type alias PotHole =
    Coordinate


type alias Table =
    { cols : Int
    , rows : Int
    , potHoles : List PotHole
    }


type alias Robot =
    { currentCoordinate : Coordinate
    , direction : Direction
    , targetCoordinate : Coordinate
    }


type alias Form =
    Coordinate


type alias Model =
    { table : Table
    , robot : Maybe Robot
    , form : Form
    , routeToTarget : List Coordinate
    }


potHoles : List PotHole
potHoles =
    [ Coordinate 2 2
    , Coordinate 2 4
    ]


initModel : Model
initModel =
    { table = Table 6 6 potHoles
    , robot = Nothing
    , form = Coordinate 0 0
    , routeToTarget = []
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel, Cmd.none )


targetCoordinate : Coordinate
targetCoordinate =
    Coordinate 4 4



-- TODO: ROUTER
-- ITERATE THROUGH ALL ADJACENTS, NOT ONLY THE FIRST ONE


getAdjacents : Table -> Coordinate -> Coordinate -> List Coordinate -> List Coordinate
getAdjacents table currentCoord finalCoord visitedCoords =
    let
        adjacents =
            [ Coordinate (currentCoord.x + 1) currentCoord.y
            , Coordinate currentCoord.x (currentCoord.y + 1)
            , Coordinate (currentCoord.x - 1) currentCoord.y
            , Coordinate currentCoord.x (currentCoord.y - 1)
            ]

        reachableAdjacents =
            List.filter (\adjacent -> validCoordinate table adjacent.x adjacent.y) adjacents

        nonVisitedCoords =
            List.filter (\adjacent -> not (List.member adjacent visitedCoords)) reachableAdjacents

        updatedVisitedCoords =
            currentCoord :: visitedCoords

        foundTarget =
            List.member finalCoord updatedVisitedCoords
    in
    if foundTarget then
        updatedVisitedCoords

    else
        case nonVisitedCoords of
            [] ->
                []

            nextCoord :: _ ->
                getAdjacents table nextCoord finalCoord updatedVisitedCoords


findPath : Table -> Coordinate -> Coordinate -> List Coordinate
findPath table currentCoord finalCoord =
    let
        adjacents =
            getAdjacents table currentCoord finalCoord []
    in
    adjacents



-- UPDATE


turnRight : Direction -> Direction
turnRight direction =
    case direction of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


turnLeft : Direction -> Direction
turnLeft direction =
    case direction of
        North ->
            West

        West ->
            South

        South ->
            East

        East ->
            North


directionToString : Direction -> String
directionToString direction =
    case direction of
        North ->
            "North"

        East ->
            "East"

        South ->
            "South"

        West ->
            "West"


isPotHole : Int -> Int -> List PotHole -> Bool
isPotHole x y potholes =
    List.any (\pothole -> x == pothole.x && y == pothole.y) potholes


validCoordinate : Table -> Int -> Int -> Bool
validCoordinate table x y =
    let
        validBoundary =
            x >= 0 && x <= table.cols - 1 && y >= 0 && y <= table.rows - 1

        notPotHole =
            not (isPotHole x y table.potHoles)
    in
    validBoundary && notPotHole


placeRobot : Table -> Int -> Int -> Direction -> Maybe Robot
placeRobot table x y direction =
    let
        currentCoordinate =
            Coordinate x y
    in
    if validCoordinate table x y then
        Just (Robot currentCoordinate direction targetCoordinate)

    else
        Nothing


moveRobot : Table -> Robot -> Robot
moveRobot table robot =
    let
        currentCoordinate =
            robot.currentCoordinate

        ( newX, newY ) =
            case robot.direction of
                North ->
                    ( currentCoordinate.x, currentCoordinate.y - 1 )

                East ->
                    ( currentCoordinate.x + 1, currentCoordinate.y )

                South ->
                    ( currentCoordinate.x, currentCoordinate.y + 1 )

                West ->
                    ( currentCoordinate.x - 1, currentCoordinate.y )

        newCoordinate =
            Coordinate newX newY

        updatedRobot =
            if validCoordinate table newX newY then
                { robot | currentCoordinate = newCoordinate }

            else
                robot
    in
    updatedRobot


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputX x ->
            let
                oldForm =
                    model.form

                updatedForm =
                    { oldForm | x = Maybe.withDefault 0 (String.toInt x) }
            in
            ( { model | form = updatedForm }, Cmd.none )

        InputY y ->
            let
                oldForm =
                    model.form

                updatedForm =
                    { oldForm | y = Maybe.withDefault 0 (String.toInt y) }
            in
            ( { model | form = updatedForm }, Cmd.none )

        Move ->
            let
                ( newModel, newCmd ) =
                    case model.robot of
                        Just currentRobot ->
                            let
                                path =
                                    findPath model.table currentRobot.currentCoordinate currentRobot.targetCoordinate
                                        |> List.reverse
                            in
                            ( { model
                                | robot = Just (moveRobot model.table currentRobot)
                                , routeToTarget = path
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )
            in
            ( newModel, newCmd )

        Place ->
            let
                newRobot =
                    placeRobot model.table model.form.x model.form.y South
            in
            ( { model
                | robot = newRobot
              }
            , Cmd.none
            )

        TurnLeft ->
            let
                ( newModel, newCmd ) =
                    case model.robot of
                        Just currentRobot ->
                            let
                                newDirection =
                                    turnLeft currentRobot.direction

                                updatedRobot =
                                    { currentRobot | direction = newDirection }
                            in
                            ( { model
                                | robot = Just updatedRobot
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )
            in
            ( newModel, newCmd )

        TurnRight ->
            let
                ( newModel, newCmd ) =
                    case model.robot of
                        Just currentRobot ->
                            let
                                newDirection =
                                    turnRight currentRobot.direction

                                updatedRobot =
                                    { currentRobot | direction = newDirection }
                            in
                            ( { model
                                | robot = Just updatedRobot
                              }
                            , Cmd.none
                            )

                        Nothing ->
                            ( model, Cmd.none )
            in
            ( newModel, newCmd )



-- VIEW


renderRobotStatus : Maybe Robot -> Html Msg
renderRobotStatus robot =
    case robot of
        Just currentRobot ->
            let
                position =
                    [ String.fromInt currentRobot.currentCoordinate.x
                    , String.fromInt currentRobot.currentCoordinate.y
                    , directionToString currentRobot.direction
                    ]
                        |> String.join " - "
            in
            div []
                [ text ("Robot position: " ++ position) ]

        Nothing ->
            div []
                [ text "No robot placed yet! " ]


renderRobotControls : Html Msg
renderRobotControls =
    div []
        [ div [] [ text "Robot Control" ]
        , div []
            [ div []
                [ input [ type_ "text", placeholder "X", onInput InputX ] []
                , input [ type_ "text", placeholder "Y", onInput InputY ] []
                , button [ onClick Place ] [ text "Place" ]
                ]
            , button [ onClick TurnLeft ] [ text "Turn Left" ]
            , button [ onClick TurnRight ] [ text "Turn Right" ]
            , button [ onClick Move ] [ text "Move" ]
            ]
        ]


renderBox : Int -> Int -> Table -> Robot -> Html Msg
renderBox posX posY table robot =
    let
        xX =
            posX * 30 + 1

        yY =
            posY * 30 + 1

        isTargetCoordinate =
            robot.targetCoordinate.x == posX && robot.targetCoordinate.y == posY

        isCurrentPosition =
            posX == robot.currentCoordinate.x && posY == robot.currentCoordinate.y

        color =
            if isCurrentPosition then
                Color.blue

            else if isTargetCoordinate then
                Color.green

            else if isPotHole posX posY table.potHoles then
                Color.red

            else
                Color.white
    in
    rect
        [ x (px (toFloat xX))
        , y (px (toFloat yY))
        , width (px 30)
        , height (px 30)
        , fill <| Fill color
        , strokeWidth (px 1)
        , stroke <| Color.black
        ]
        []


renderCols : Int -> Int -> Table -> Robot -> List (Html Msg)
renderCols x y table robot =
    if x < 0 then
        []

    else
        renderBox x y table robot :: renderCols (x - 1) y table robot


renderBoxes : Int -> Int -> Table -> Robot -> List (Html Msg)
renderBoxes x y table robot =
    let
        cols =
            renderCols x y table robot
    in
    if y < 0 then
        []

    else
        List.append cols (renderBoxes x (y - 1) table robot)


renderTable : Table -> Robot -> Html Msg
renderTable table robot =
    let
        svgHeight =
            table.rows * 35
    in
    div []
        [ text ("Table: [" ++ String.fromInt table.cols ++ "x" ++ String.fromInt table.rows ++ "]")
        , svg [ viewBox 0 0 800 (toFloat svgHeight) ]
            (renderBoxes (table.cols - 1) (table.rows - 1) table robot)
        ]


renderRoute : Coordinate -> Html Msg
renderRoute coord =
    div []
        [ text ("x: " ++ String.fromInt coord.x ++ " y: " ++ String.fromInt coord.y) ]


renderView : Model -> Html Msg
renderView model =
    let
        renderBoard =
            case model.robot of
                Just currentRobot ->
                    renderTable model.table currentRobot

                Nothing ->
                    text ""
    in
    div []
        [ renderBoard
        , renderRobotControls
        , renderRobotStatus model.robot
        , div []
            [ hr [] []
            , div [] [ text "Possible Route" ]
            , div [] (List.map renderRoute model.routeToTarget)
            ]
        ]


view : Model -> Document Msg
view model =
    { title = "Toy Robot"
    , body = [ renderView model ]
    }



-- MAIN


type alias Flags =
    {}


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
