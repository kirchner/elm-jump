{- elm-jump - some platformer idea
   Copyright © 2016 Fabian Kirchner

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}


module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Keyboard
import Math.Vector2 as Vec2
    exposing
        ( Vec2
        , add
        , getX
        , getY
        , scale
        , toTuple
        , vec2
        )
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Time exposing (Time)
import Engine
import Camera exposing (defaultCamera)


-- STATE


type alias State =
    { position : Vec2
    , movement : Maybe Direction
    , velocity : Float
    , ground : List Line
    , tcamera : Camera.T
    }


defaultState : State
defaultState =
    { position = vec2 500 50
    , movement = Nothing
    , velocity = 0
    , ground =
        [ { left = vec2 0 300
          , right = vec2 600 400
          }
        , { left = vec2 400 200
          , right = vec2 600 100
          }
        ]
    , tcamera =
          Camera.defaultT
    }


type alias Line =
    { left : Vec2
    , right : Vec2
    }



-- STEP


{-| Propagate the State by the given Time.  This includes applying
gravity force and the movements.
-}
physics : Time -> State -> State
physics dt state =
    state
        |> movement dt
        |> gravity dt


gravity : Time -> State -> State
gravity dt state =
    let
        acceleration =
            0.0004

        newVelocity =
            acceleration * (Time.inMilliseconds dt) + state.velocity

        newPosition =
            vec2 0 1
                |> scale (newVelocity * (Time.inMilliseconds dt))
                |> add state.position
    in
        { state
            | velocity = newVelocity
            , position = newPosition
        }


movement : Time -> State -> State
movement dt state =
    let
        speed =
            0.1

        direction =
            case state.movement of
                Just Left ->
                    vec2 -1 0

                Just Right ->
                    vec2 1 0

                Nothing ->
                    vec2 0 0

        newPosition =
            direction
                |> scale (speed * (Time.inMilliseconds dt))
                |> add state.position
    in
        { state | position = newPosition }


{-| Check for collisions and adapt the state accordingly.
-}
collision : Time -> State -> State -> State
collision dt oldState newState =
    let
        ox =
            getX oldState.position

        oy =
            getY oldState.position

        lx line =
            getX line.left

        ly line =
            getY line.left

        rx line =
            getX line.right

        ry line =
            getY line.right

        iy line =
            (ox - lx line)
                * (ry line - ly line)
                / (rx line - lx line)
                + ly line

        adjust line newPosition =
            if
                ((oy - 2) <= iy line)
                    && ((getY newPosition + 2) > iy line)
                    && (ox >= (lx line))
                    && (ox <= (rx line))
            then
                vec2 (getX newPosition) (iy line)
            else
                newPosition

        newPosition =
            List.foldr adjust newState.position newState.ground
    in
        { newState | position = newPosition }


{-| Compute new State.
-}
step : Time -> State -> State
step dt state =
    state
    |> physics dt
    |> collision dt state
    |> camera dt -- TODO: currentTime


camera : Time -> State -> State
camera dt state =
    let
        currentTime =
            state.tcamera.lastFrame + dt
    in
        { state
            | tcamera =
                Camera.step currentTime state.tcamera
                |> Camera.retarget currentTime
                   ( let
                         pointOfInterest =
                             vec2 500 150

                         p =
                             Vec2.length (Vec2.sub state.position pointOfInterest) <= 100
                     in
                         if p then
                                 { defaultCamera
                                     | position = state.position
                                     , scale = 2.5
                                 }
                             else
                                 { defaultCamera
                                     | position = state.position
                                     , scale = 1
                                 }
                   )
        }



-- DRAW


draw : Time -> State -> Html m
draw dt state =
    let
        newState =
            step dt state

        ( x, y ) =
            toTuple newState.position
    in
        Svg.svg
            [ Svg.width "600"
            , Svg.height "400"
            , Svg.viewBox "0 0 600 400"
            , Svg.transform (Camera.transform state.tcamera.lastCamera)
            ]
            [ Svg.rect
                [ Svg.x <| toString (x - 10)
                , Svg.y <| toString (y - 20)
                , Svg.width "20"
                , Svg.height "20"
                , Svg.fill "green"
                ]
                []
            , drawGround state
            ]
--        |> \svg ->
--            Html.div []
--            [ svg
--            , Html.div [] [ Html.text (toString state.tcamera) ]
--            , Html.div [] [ Html.text (toString state.position) ]
--            , Html.div [] [ Html.text (Camera.transform state.tcamera.lastCamera) ]
--            ]


drawGround : State -> Svg m
drawGround state =
    let
        drawLine line =
            Svg.line
                [ Svg.x1 <| toString <| getX line.left
                , Svg.y1 <| toString <| getY line.left
                , Svg.x2 <| toString <| getX line.right
                , Svg.y2 <| toString <| getY line.right
                , Svg.width "1"
                , Svg.stroke "brown"
                ]
                []
    in
        Svg.g [] <| List.map drawLine state.ground



-- CMD


type Cmd
    = NoOp
    | Jump
    | Move Direction
    | Halt


type Direction
    = Left
    | Right



-- EXECUTE


execute : Cmd -> State -> State
execute cmd state =
    case cmd of
        NoOp ->
            state

        Jump ->
            { state | velocity = -0.3 }

        Move direction ->
            { state | movement = Just direction }

        Halt ->
            { state | movement = Nothing }



-- KEYBIND


bind : Keyboard.KeyCode -> Bool -> Cmd
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
                Halt

            -- right
            39 ->
                Halt

            _ ->
                NoOp



-- MAIN


type alias Jump =
    Engine.GameLogic State Cmd


jump : Jump
jump =
    { defaultState = defaultState
    , execute = execute
    , bind = bind
    , step = step
    , draw = draw
    }


main : Program Never (Engine.Model State Cmd) Engine.Msg
main =
    Engine.engine jump
