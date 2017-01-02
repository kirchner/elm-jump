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
import Math.Vector2
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


-- STATE


type alias State =
    { position : Vec2
    , velocity : Float
    , ground : List Line
    }


defaultState : State
defaultState =
    { position = vec2 500 50
    , velocity = 0
    , ground =
        [ { left = vec2 0 300
          , right = vec2 600 400
          }
        , { left = vec2 400 200
          , right = vec2 600 100
          }
        ]
    }


type alias Line =
    { left : Vec2
    , right : Vec2
    }



-- STEP


step : Time -> State -> State
step dt state =
    let
        newPosition =
            scale (state.velocity * Time.inMilliseconds dt) (vec2 1 0)
                |> add state.position
    in
        stepFall dt { state | position = newPosition }


stepFall : Time -> State -> State
stepFall dt state =
    let
        newPosition =
            vec2
                (getX state.position)
                (0.2 * (Time.inMilliseconds dt) + getY state.position)
    in
        collide state.position { state | position = newPosition }


collide : Vec2 -> State -> State
collide oldPosition state =
    let
        iy line =
            (getX oldPosition - getX line.left)
                * (getY line.right - getY line.left)
                / (getX line.right - getX line.left)
                + getY line.left

        adjust line newPosition =
            if
                ((getY oldPosition - 2) <= iy line)
                    && ((getY newPosition + 2) > iy line)
                    && (getX oldPosition >= (getX line.left))
                    && (getX oldPosition <= (getX line.right))
            then
                vec2 (getX newPosition) (iy line)
            else
                newPosition
    in
        { state | position = List.foldr adjust state.position state.ground }



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
            ]
            [ Svg.rect
                [ Svg.x <| toString x
                , Svg.y <| toString (y - 20)
                , Svg.width "20"
                , Svg.height "20"
                , Svg.fill "green"
                ]
                []
            , drawGround state
            ]


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

        --        KeyDown keycode ->
        --            case keycode of
        --                -- spacebar
        --                32 ->
        --                    update Jump model
        --
        --                -- left
        --                37 ->
        --                    update (Move Left) model
        --
        --                -- right
        --                39 ->
        --                    update (Move Right) model
        --
        --                _ ->
        --                    model ! []
        --
        --        KeyUp keycode ->
        --            case keycode of
        --                -- left
        --                37 ->
        --                    update Halt model
        --
        --                -- right
        --                39 ->
        --                    update Halt model
        --
        --                _ ->
        --                    model ! []
        Jump ->
            state

        Move direction ->
            let
                newVelocity =
                    case direction of
                        Left ->
                            -0.1

                        Right ->
                            0.1

                newState =
                    { state | velocity = newVelocity }
            in
                newState

        Halt ->
            let
                newVelocity =
                    0

                newState =
                    { state | velocity = newVelocity }
            in
                newState



-- MAIN


type alias Jump =
    Engine.GameLogic State Cmd


jump : Jump
jump =
    { defaultState = defaultState
    , execute = execute
    , step = step
    , draw = draw
    }


main : Program Never (Engine.Model State Cmd) Engine.Msg
main =
    Engine.engine jump
