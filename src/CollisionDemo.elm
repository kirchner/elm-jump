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

import Dict
import Html exposing (Html)
import Html.Events as Events
import Keyboard
import Math.Vector2 as Vec2
    exposing
        ( Vec2
        , add
        , getX
        , getY
        , length
        , scale
        , sub
        , toTuple
        , vec2
        )
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Time exposing (Time)
import Engine
import Camera exposing (defaultCamera)
import Player exposing (..)
import Http
import Json.Decode as Json
import Platform.Cmd as Cmd
import Task


-- STATE


type alias State =
    { movement : Maybe Direction
    , ground : List Line
    , tcamera : Camera.T
    , curPlayers : List Player
    , prevPlayers : List Player
    , activePlayer : Int
    }


defaultState : State
defaultState =
    { movement = Nothing
    , ground =
        [ { left = vec2 0 100
          , right = vec2 600 200
          }
        ]
    , tcamera =
        Camera.defaultT
    , curPlayers =
        [ { defaultPlayer
            | positions =
                { a = vec2 300 0
                , b = vec2 300 20
                , c = vec2 320 20
                , d = vec2 320 0
                }
            , width = 20
            , height = 20
            , color = "#ffcc00"
            , active = True
          }
        ]
    , prevPlayers =
        [ { defaultPlayer
            | positions =
                { a = vec2 300 0
                , b = vec2 300 20
                , c = vec2 320 20
                , d = vec2 320 0
                }
            , width = 20
            , height = 20
            , color = "#ffcc00"
            , active = True
          }
        ]
    , activePlayer = 0
    }


type alias Line =
    { left : Vec2
    , right : Vec2
    }



-- STEP


stepPlayers : Time -> State -> State
stepPlayers dt state =
    let
        lineSegment line =
            { a = line.left
            , b = line.right
            }

        lines =
            List.concat
                [ state.ground
                  --, state.players
                  --    |> List.map
                  --        (\player ->
                  --            { left = player.position |> add (vec2 (-10) (-player.height))
                  --            , right = player.position |> add (vec2 10 (-player.height))
                  --            }
                  --        )
                ]

        lineSegments =
            List.map lineSegment lines

        newPlayers =
            List.map2
                (stepPlayer dt lineSegments)
                state.curPlayers
                state.prevPlayers
    in
        { state
            | curPlayers = newPlayers
            , prevPlayers = state.curPlayers
        }


{-| Compute new State.
-}
step : Time -> State -> State
step dt state =
    state
        |> stepPlayers dt
        |> camera dt


camera : Time -> State -> State
camera dt state =
    let
        currentTime =
            state.tcamera.lastFrame + dt

        player =
            state.curPlayers
                |> List.drop state.activePlayer
                |> List.head
                |> Maybe.withDefault defaultPlayer
    in
        { state
            | tcamera =
                Camera.step currentTime state.tcamera
                    |> Camera.retarget currentTime
                        (let
                            pointOfInterest =
                                vec2 500 150

                            position =
                                player.positions.a

                            p =
                                Vec2.length (Vec2.sub position pointOfInterest) <= 100
                         in
                            if p then
                                { defaultCamera
                                    | position = position
                                    , scale = 2.5
                                }
                            else
                                { defaultCamera
                                    | position = position
                                    , scale = 1
                                }
                        )
        }



-- DRAW


draw : Time -> State -> Html Cmd
draw dt state =
    let
        newState =
            step dt state
    in
        Svg.svg
            [ Svg.width "600"
            , Svg.height "400"
            , Svg.viewBox "0 0 600 400"
            , Svg.transform (Camera.transform state.tcamera.lastCamera)
            ]
            (List.concat
                [ state.curPlayers
                    |> List.indexedMap (,)
                    |> List.sortBy
                        (\( i, r ) ->
                            if i == state.activePlayer then
                                1
                            else
                                0
                        )
                    |> List.map (\( i, r ) -> drawPlayer r)
                    |> List.concat
                , [ drawGround state ]
                ]
            )
            |> \svg ->
                Html.div []
                    [ svg
                    , Html.button [ Events.onClick (Load "level0.json") ] [ Html.text "level 0" ]
                    , Html.button [ Events.onClick (Load "level1.json") ] [ Html.text "level 1" ]
                    ]


drawPlayer : Player -> List (Svg m)
drawPlayer player =
    let
        drawCorner position =
            Svg.circle
                [ Svg.cx <| toString <| getX <| position
                , Svg.cy <| toString <| getY <| position
                , Svg.r "2"
                , Svg.fill player.color
                ]
                []
    in
        (List.map (\a -> drawCorner (a player.positions)) [ .a, .b, .c, .d ])


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

        lines =
            List.concat
                [ state.ground
                  --, state.players
                  --    |> List.map
                  --        (\player ->
                  --            { left = player.position |> add (vec2 (-10) (-player.height))
                  --            , right = player.position |> add (vec2 10 (-player.height))
                  --            }
                  --        )
                ]
    in
        Svg.g [] <| List.map drawLine lines



-- CMD


type Cmd
    = NoOp
    | Jump
    | Move Direction
    | Halt
    | Cycle
    | Load String
    | LoadOk (Result Http.Error (List Line))


type Direction
    = Left
    | Right



-- EXECUTE


execute : Cmd -> State -> ( State, Cmd.Cmd Cmd )
execute cmd state =
    case cmd of
        NoOp ->
            state ! []

        Jump ->
            state ! []

        Move direction ->
            state ! []

        Halt ->
            state ! []

        Cycle ->
            let
                activePlayer =
                    state.activePlayer
                        + 1
                        |> flip (%) (List.length state.curPlayers)
            in
                { state
                    | activePlayer = activePlayer
                }
                    ! []

        Load fn ->
            let
                parse1 xs =
                    case xs of
                        x :: y :: xs_ ->
                            vec2 (10 * x) (-10 * y + 500) :: parse1 xs_

                        _ ->
                            []

                parse2 xs =
                    case xs of
                        u :: v :: xs_ ->
                            { left = u, right = v } :: parse2 (v :: xs_)

                        _ ->
                            []

                parse =
                    List.map parse1 >> List.map parse2 >> List.concat

                decodeLevel =
                    Json.list (Json.list (Json.float)) |> Json.map parse
            in
                state ! [ Http.get fn decodeLevel |> Http.send LoadOk ]

        LoadOk (Ok ground) ->
            { state
                | ground = ground
            }
                ! []

        LoadOk (Err _) ->
            state ! []



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

            -- tab
            9 ->
                Cycle

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
    , init = init
    }


init =
    ( defaultState, Cmd.none )


main : Program Never (Engine.Model State Cmd) (Engine.Msg Cmd)
main =
    Engine.engine jump
