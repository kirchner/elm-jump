module Main exposing (..)

import Animation exposing (..)
import Dict exposing (Dict)
import Ease
import Engine
import Keyboard
import Html exposing (Html)
import Math.Vector2 exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Time exposing (Time)


type alias Recursia =
    Engine.GameLogic State Action


recursia : Recursia
recursia =
    { defaultState = defaultState
    , execute = execute
    , bind = bind
    , step = step
    , init = init
    , draw = draw
    }


type Action
    = NoOp
    | Jump
    | Move Direction
    | Stop


main : Program Never (Engine.Model State Action) (Engine.Msg Action)
main =
    Engine.engine recursia



-- STATE


type alias State =
    { level : Level
    , player : Player
    }


defaultState : State
defaultState =
    --{ levels = Dict.empty
    --, currentLevel = Nothing
    --}
    testState


testState : State
testState =
    { level = testLevel
    , player = defaultPlayer
    }


init : ( State, Cmd Action )
init =
    ( testState, Cmd.none )


{-| All Levels are 640x640.
-}
type alias Level =
    { ground : List Box
    }


testLevel : Level
testLevel =
    { ground =
        [ { upperLeft = vec2 320 200
          , lowerRight = vec2 600 240
          }
        , { upperLeft = vec2 80 400
          , lowerRight = vec2 260 440
          }
        ]
    }


type alias Box =
    { upperLeft : Vec2
    , lowerRight : Vec2
    }


{-| The players bounding box is 40x40.  The Position is the lower middle
point.
-}
type alias Player =
    { curPosition : Vec2
    , prevPosition : Vec2
    }


defaultPlayer =
    { curPosition = vec2 220 200
    , prevPosition = vec2 220 200
    }


type Direction
    = Left
    | Right



-- STEP


step : Time -> State -> State
step dt state =
    { state
        | player =
            stepPlayer dt
                state.level
                state.player
    }


stepPlayer : Time -> Level -> Player -> Player
stepPlayer dt level player =
    let
        propagatedPlayer =
            propagetePlayer dt player

        actualPlayer =
            constraintPlayer level.ground propagatedPlayer player
    in
        actualPlayer


propagetePlayer : Time -> Player -> Player
propagetePlayer dt player =
    let
        acceleration =
            vec2 0 0.01

        newPosition =
            add
                (sub (scale 2 player.curPosition) player.prevPosition)
                (scale (dt + dt) acceleration)
    in
        { player
            | curPosition = newPosition
            , prevPosition = player.curPosition
        }


constraintPlayer : List Box -> Player -> Player -> Player
constraintPlayer ground newPlayer curPlayer =
    constraintBoundingBox newPlayer


constraintBoundingBox : Player -> Player
constraintBoundingBox player =
    let
        ( x, y ) =
            toTuple player.curPosition

        newX =
            bindTo 20 620 x

        newY =
            bindTo 40 640 y

        bindTo a b t =
            if t <= a then
                a
            else if t >= b then
                b
            else
                t
    in
        { player
            | curPosition = vec2 newX newY
        }



-- EXECUTE


execute : Action -> State -> ( State, Cmd Action )
execute action state =
    case action of
        NoOp ->
            state ! []

        Jump ->
            let
                delta =
                    vec2 0 10

                newPlayer =
                    state.player
                        |> (\player ->
                                { player
                                    | prevPosition =
                                        add player.prevPosition delta
                                }
                           )
            in
                { state
                    | player = newPlayer
                }
                    ! []

        Move direction ->
            let
                delta =
                    case direction of
                        Left ->
                            vec2 5 0

                        Right ->
                            vec2 -5 0

                newPlayer =
                    state.player
                        |> (\player ->
                                { player
                                    | prevPosition =
                                        add player.prevPosition delta
                                }
                           )
            in
                { state
                    | player = newPlayer
                }
                    ! []

        Stop ->
            let
                newPlayer =
                    state.player
                        |> (\player ->
                                { player
                                    | prevPosition =
                                        vec2
                                            (getX player.curPosition)
                                            (getY player.prevPosition)
                                }
                           )
            in
                { state
                    | player = newPlayer
                }
                    ! []



-- KEYBINDING


bind : Keyboard.KeyCode -> Bool -> Action
bind keycode pressed =
    if pressed then
        case keycode of
            -- spacebar
            32 ->
                Jump

            -- left
            37 ->
                Move Left

            -- right
            39 ->
                Move Right

            _ ->
                NoOp
    else
        case keycode of
            -- left
            37 ->
                Stop

            -- right
            39 ->
                Stop

            _ ->
                NoOp



-- DRAW


draw : Time -> State -> Html Action
draw dt state =
    Svg.svg
        [ Svg.width "400"
        , Svg.height "400"
        , Svg.viewBox "-10 -10 700 700"
        ]
        [ drawLevel state.level
        , drawPlayer state.player
        ]


drawLevel : Level -> Svg a
drawLevel level =
    let
        boundingBox =
            Svg.rect
                [ Svg.x "0"
                , Svg.y "0"
                , Svg.width "640"
                , Svg.height "640"
                , Svg.fill "#f4c6c6"
                , Svg.stroke "#9f1f66"
                , Svg.strokeWidth "3"
                ]
                []

        boxes =
            List.map drawBox level.ground
    in
        Svg.g
            []
            [ boundingBox
            , Svg.g [] boxes
            ]


drawBox : Box -> Svg a
drawBox box =
    let
        ( x, y ) =
            toTuple box.upperLeft

        ( width, height ) =
            toTuple <| sub box.lowerRight box.upperLeft
    in
        Svg.rect
            [ Svg.x <| toString x
            , Svg.y <| toString y
            , Svg.width <| toString width
            , Svg.height <| toString height
            , Svg.fill "#c54173"
            , Svg.stroke "#9f1f66"
            , Svg.strokeWidth "3"
            , Svg.rx "6"
            , Svg.ry "6"
            ]
            []


drawPlayer : Player -> Svg a
drawPlayer player =
    let
        ( x, y ) =
            toTuple <| add player.curPosition (vec2 -20 -40)

        ( width, height ) =
            ( 40, 40 )
    in
        Svg.rect
            [ Svg.x <| toString x
            , Svg.y <| toString y
            , Svg.width <| toString width
            , Svg.height <| toString height
            , Svg.fill "#db4e70"
            , Svg.stroke "black"
            , Svg.strokeWidth "3"
            , Svg.rx "6"
            , Svg.ry "6"
            ]
            []
