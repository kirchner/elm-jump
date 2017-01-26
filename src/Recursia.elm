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
        [ { upperLeft = vec2 0 40
          , lowerRight = vec2 400 80
          }
        , { upperLeft = vec2 320 200
          , lowerRight = vec2 640 240
          }
        , { upperLeft = vec2 0 400
          , lowerRight = vec2 260 640
          }
        , { upperLeft = vec2 260 500
          , lowerRight = vec2 300 640
          }
        , { upperLeft = vec2 300 550
          , lowerRight = vec2 380 640
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
    { position : Vec2
    , velocity : Vec2
    , move : Maybe ( Direction, Float )
    }


defaultPlayer =
    { position = vec2 220 200
    , velocity = vec2 0 0
    , move = Nothing
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
            vec2 0 0.001

        newVelocity =
            add player.velocity (scale dt acceleration)

        newPosition =
            add player.position (scale dt newVelocity)
    in
        { player
            | position = newPosition
            , velocity = newVelocity
        }


constraintPlayer : List Box -> Player -> Player -> Player
constraintPlayer ground newPlayer curPlayer =
    constraintBoundingBox <|
        List.foldr constraintBox (constraintBoundingBox newPlayer) ground


constraintBoundingBox : Player -> Player
constraintBoundingBox player =
    let
        ( x, y ) =
            toTuple player.position

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

        newVelocity =
            if newY == 640 then
                vec2 (getX player.velocity) 0
            else
                player.velocity
    in
        { player
            | position = vec2 newX newY
            , velocity = newVelocity
        }


constraintBox : Box -> Player -> Player
constraintBox box player =
    let
        ( x, y ) =
            toTuple player.position

        ( playerCenterX, playerCenterY ) =
            ( x, y - 20 )

        ( playerWidth, playerHeight ) =
            ( 40, 40 )

        ( qx, qy ) =
            toTuple box.upperLeft

        ( px, py ) =
            toTuple box.lowerRight

        ( boxCenterX, boxCenterY ) =
            ( (qx + px) / 2, (qy + py) / 2 )

        ( boxWidth, boxHeight ) =
            ( px - qx, py - qy )

        xOverlap =
            abs (playerCenterX - boxCenterX) - (playerWidth + boxWidth) / 2

        yOverlap =
            abs (playerCenterY - boxCenterY) - (playerHeight + boxHeight) / 2
    in
        if (xOverlap < 0) && (yOverlap < 0) then
            -- box and player intersect
            if xOverlap >= yOverlap then
                if playerCenterX <= boxCenterX then
                    -- move to the left
                    { player
                        | position =
                            sub player.position (vec2 xOverlap 0)
                    }
                else
                    -- move to the right
                    { player
                        | position =
                            sub player.position (vec2 xOverlap 0)
                    }
            else if playerCenterY <= boxCenterY then
                -- move up
                { player
                    | position =
                        add player.position (vec2 0 yOverlap)
                    , velocity =
                        vec2 (getX player.velocity) 0
                }
            else
                -- move down
                { player
                    | position =
                        sub player.position (vec2 0 yOverlap)
                    , velocity =
                        vec2 (getX player.velocity) 0
                }
        else
            -- box and player do not intersect
            player



-- EXECUTE


execute : Action -> State -> ( State, Cmd Action )
execute action state =
    case action of
        NoOp ->
            state ! []

        Jump ->
            let
                jumpSpeed =
                    if getY state.player.velocity == 0 then
                        -0.5
                    else
                        0

                newPlayer =
                    state.player
                        |> (\player ->
                                { player
                                    | velocity =
                                        add player.velocity (vec2 0 jumpSpeed)
                                }
                           )
            in
                { state
                    | player = newPlayer
                }
                    ! []

        Move newDirection ->
            let
                delta =
                    case newDirection of
                        Left ->
                            vec2 -0.3 0

                        Right ->
                            vec2 0.3 0

                newPlayer =
                    state.player
                        |> (\player ->
                                case player.move of
                                    Just ( prevDirection, duration ) ->
                                        if prevDirection == newDirection then
                                            player
                                        else
                                            { player
                                                | velocity =
                                                    vec2 (getX delta) (getY player.velocity)
                                                , move = Just ( newDirection, 0 )
                                            }

                                    Nothing ->
                                        { player
                                            | velocity =
                                                vec2 (getX delta) (getY player.velocity)
                                            , move = Just ( newDirection, 0 )
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
                                    | velocity =
                                        vec2 0 (getY player.velocity)
                                    , move = Nothing
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
        [ Svg.width "660"
        , Svg.height "660"
        , Svg.viewBox "-10 -10 660 660"
        ]
        [ drawLevel state.level
        , drawPlayer state.player
        ]


drawLevel : Level -> Svg a
drawLevel level =
    let
        boundingBox =
            Svg.rect
                [ Svg.x "-2"
                , Svg.y "-2"
                , Svg.width "644"
                , Svg.height "644"
                , Svg.fill "#f4c6c6"
                , Svg.stroke "#c54173"
                , Svg.strokeWidth "4"
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
            , Svg.strokeWidth "0"
            , Svg.rx "0"
            , Svg.ry "0"
            ]
            []


drawPlayer : Player -> Svg a
drawPlayer player =
    let
        ( x, y ) =
            toTuple <| add player.position (vec2 -20 -40)

        ( width, height ) =
            ( 40, 40 )
    in
        Svg.rect
            [ Svg.x <| toString x
            , Svg.y <| toString y
            , Svg.width <| toString width
            , Svg.height <| toString height
            , Svg.fill "#db4e70"
            , Svg.strokeWidth "0"
            , Svg.rx "6"
            , Svg.ry "6"
            ]
            []
