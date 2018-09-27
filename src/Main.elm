module Main exposing (Msg(..), main, update, view)

import Browser exposing (Document, document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



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


type alias Table =
    { cols : Int
    , rows : Int
    }


type alias Robot =
    { x : Int
    , y : Int
    , direction : Direction
    }


type alias Model =
    { table : Table
    , robot : Maybe Robot
    , x : Int
    , y : Int
    }


initModel : Model
initModel =
    { table = Table 4 4
    , robot = Nothing
    , x = 0
    , y = 0
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initModel, Cmd.none )



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


validPosition : Table -> Int -> Int -> Bool
validPosition table x y =
    x >= 0 && x <= table.cols && y >= 0 && y <= table.rows


placeRobot : Table -> Int -> Int -> Direction -> Maybe Robot
placeRobot table x y direction =
    if validPosition table x y then
        Just (Robot x y direction)

    else
        Nothing


moveRobot : Table -> Robot -> Robot
moveRobot table robot =
    let
        ( newX, newY ) =
            case robot.direction of
                North ->
                    ( robot.x, robot.y + 1 )

                East ->
                    ( robot.x + 1, robot.y )

                South ->
                    ( robot.x, robot.y - 1 )

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
            ( { model | x = Maybe.withDefault 0 (String.toInt x) }, Cmd.none )

        InputY y ->
            ( { model | y = Maybe.withDefault 0 (String.toInt y) }, Cmd.none )

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
                    placeRobot model.table model.x model.y North
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


renderTable : Table -> Html Msg
renderTable table =
    div []
        [ text ("Table: [" ++ String.fromInt table.cols ++ "x" ++ String.fromInt table.rows ++ "]") ]


renderView : Model -> Html Msg
renderView model =
    div []
        [ renderTable model.table
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
