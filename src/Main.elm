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
    | Report
    | InputX String
    | InputY String


type alias PotHole =
    { x : Int
    , y : Int
    }


type alias Table =
    { cols : Int
    , rows : Int
    , potHoles : List PotHole
    }


type alias Coordinate =
    { x : Int
    , y : Int
    }


type alias Robot =
    { x : Int
    , y : Int
    , direction : Direction
    , targetCoordinate : Coordinate
    }


type alias Form =
    { x : Int
    , y : Int
    }


type alias Model =
    { table : Table
    , robot : Maybe Robot
    , form : Form
    }


potHoles : List PotHole
potHoles =
    [ PotHole 2 2
    , PotHole 2 4
    ]


initModel : Model
initModel =
    { table = Table 6 6 potHoles
    , robot = Nothing
    , form = Form 0 0
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel, Cmd.none )


targetCoordinate : Coordinate
targetCoordinate =
    Coordinate 4 4



-- TODO: ROUTER
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


validPosition : Table -> Int -> Int -> Bool
validPosition table x y =
    let
        validBoundary =
            x >= 0 && x <= table.cols - 1 && y >= 0 && y <= table.rows - 1

        notPotHole =
            not (isPotHole x y table.potHoles)
    in
    validBoundary && notPotHole


placeRobot : Table -> Int -> Int -> Direction -> Maybe Robot
placeRobot table x y direction =
    if validPosition table x y then
        Just (Robot x y direction targetCoordinate)

    else
        Nothing


moveRobot : Table -> Robot -> Robot
moveRobot table robot =
    let
        ( newX, newY ) =
            case robot.direction of
                North ->
                    ( robot.x, robot.y - 1 )

                East ->
                    ( robot.x + 1, robot.y )

                South ->
                    ( robot.x, robot.y + 1 )

                West ->
                    ( robot.x - 1, robot.y )

        updatedRobot =
            if validPosition table newX newY then
                { robot | x = newX, y = newY }

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
                            ( { model
                                | robot = Just (moveRobot model.table currentRobot)
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

        Report ->
            ( model, Cmd.none )



-- VIEW


renderRobotStatus : Maybe Robot -> Html Msg
renderRobotStatus robot =
    case robot of
        Just currentRobot ->
            let
                position =
                    [ String.fromInt currentRobot.x
                    , String.fromInt currentRobot.y
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

        color =
            if posX == robot.x && posY == robot.y then
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


renderTable : Table -> Robot -> Html Msg
renderTable table robot =
    let
        svgHeight =
            table.rows * 35
    in
    div []
        [ text ("Table: [" ++ String.fromInt table.cols ++ "x" ++ String.fromInt table.rows ++ "]")
        , svg [ viewBox 0 0 800 (toFloat svgHeight) ]
            [ renderBox 0 0 table robot
            , renderBox 1 0 table robot
            , renderBox 2 0 table robot
            , renderBox 3 0 table robot
            , renderBox 4 0 table robot
            , renderBox 5 0 table robot
            , renderBox 0 1 table robot
            , renderBox 1 1 table robot
            , renderBox 2 1 table robot
            , renderBox 3 1 table robot
            , renderBox 4 1 table robot
            , renderBox 5 1 table robot
            , renderBox 0 2 table robot
            , renderBox 1 2 table robot
            , renderBox 2 2 table robot
            , renderBox 3 2 table robot
            , renderBox 4 2 table robot
            , renderBox 5 2 table robot
            , renderBox 0 3 table robot
            , renderBox 1 3 table robot
            , renderBox 2 3 table robot
            , renderBox 3 3 table robot
            , renderBox 4 3 table robot
            , renderBox 5 3 table robot
            , renderBox 0 4 table robot
            , renderBox 1 4 table robot
            , renderBox 2 4 table robot
            , renderBox 3 4 table robot
            , renderBox 4 4 table robot
            , renderBox 5 4 table robot
            , renderBox 0 5 table robot
            , renderBox 1 5 table robot
            , renderBox 2 5 table robot
            , renderBox 3 5 table robot
            , renderBox 4 5 table robot
            , renderBox 5 5 table robot
            ]
        ]


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
