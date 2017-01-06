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
import Player exposing (Player, defaultPlayer)
import Http
import Json.Decode as Json
import Platform.Cmd as Cmd
import Task


-- STATE


type alias State =
    { movement : Maybe Direction
    , ground : List Line
    , tcamera : Camera.T
    , players : List Player
    , activePlayer : Int
    }


defaultState : State
defaultState =
    { movement = Nothing
    , ground =
          let
              parse1 xs =
                  case xs of
                      (x::y::xs_) ->
                          vec2 (10*x) (-10*y + 500) :: parse1 xs_
                      _ ->
                          []

              parse2 xs =
                  case xs of
                      (u::v::xs_) ->
                          { left = u, right = v } :: parse2 (v::xs_)
                      _ ->
                          []

              parse =
                  parse1 >> parse2
          in
              parse [0.0, 0.0, 2.3069236278533936, 8.262032508850098, 8.028361320495605, 15.044477462768555, 16.468671798706055, 20.340553283691406, 26.932214736938477, 24.143482208251953, 38.723350524902344, 26.446483612060547, 51.14643859863281, 27.24277687072754, 63.505836486816406, 26.525583267211914, 75.10590362548828, 24.288122177124023, 85.2509994506836, 20.52361297607422, 93.2454833984375, 15.225276947021484, 98.39371490478516, 8.386334419250488, 100.0, 0.0, 0 ]
    , tcamera =
        Camera.defaultT
    , players =
        [ { defaultPlayer
            | position = vec2 100 100
            , height = 20
            , color = "#ffcc00"
            , active = True
          }
        , { defaultPlayer
            | position = vec2 200 100
            , height = 60
            , color = "#d400aa"
          }
        , { defaultPlayer
            | position = vec2 300 100
            , height = 40
            , color = "#0066ff"
          }
        , { defaultPlayer
            | position = vec2 400 100
            , height = 40
            , color = "#00d400"
          }
        ]
    , activePlayer = 0
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

        players =
            state.players
                |> List.map
                    (\player ->
                        let
                            newVelocity =
                                acceleration * (Time.inMilliseconds dt) + player.velocity

                            newPosition =
                                vec2 0 1
                                    |> scale (newVelocity * (Time.inMilliseconds dt))
                                    |> add player.position
                        in
                            { player
                                | position = newPosition
                                , velocity = newVelocity
                            }
                    )
    in
        { state
            | players = players
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

        players =
            state.players
                |> List.indexedMap
                    (\i player ->
                        if i == state.activePlayer then
                            let
                                newPosition =
                                    direction
                                        |> scale (speed * (Time.inMilliseconds dt))
                                        |> add player.position
                            in
                                { player
                                    | position = newPosition
                                }
                        else
                            player
                    )
    in
        { state | players = players }


{-| Check for collisions and adapt the state accordingly.
-}
collision : Time -> State -> State -> State
collision dt oldState newState =
    let
        lines =
            List.concat
                [ newState.ground
                , oldState.players
                    |> List.map
                        (\player ->
                            { left = player.position |> add (vec2 (-10) (-player.height))
                            , right = player.position |> add (vec2 10 (-player.height))
                            }
                        )
                ]

        players =
            newState.players
                |> List.map2 (,) oldState.players
                |> List.map
                    (\( oldPlayer, newPlayer ) ->
                        let
                            ox =
                                getX oldPlayer.position

                            oy =
                                getY oldPlayer.position

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
                                List.foldr adjust newPlayer.position lines
                        in
                            { newPlayer
                                | position = newPosition
                            }
                    )
    in
        { newState | players = players }


{-| Compute new State.
-}
step : Time -> State -> State
step dt state =
    state
        |> physics dt
        |> collision dt state
        |> camera dt



-- TODO: currentTime


camera : Time -> State -> State
camera dt state =
    let
        currentTime =
            state.tcamera.lastFrame + dt

        player =
            state.players
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

                            p =
                                Vec2.length (Vec2.sub player.position pointOfInterest) <= 100
                         in
                            if p then
                                { defaultCamera
                                    | position = player.position
                                    , scale = 2.5
                                }
                            else
                                { defaultCamera
                                    | position = player.position
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
                [ state.players
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
    [ Svg.rect
        [ Svg.x <| toString (getX player.position - 10)
        , Svg.y <| toString (getY player.position - player.height)
        , Svg.width "20"
        , Svg.height (player.height |> toString)
        , Svg.fill player.color
        ]
        []
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

        lines =
            List.concat
                [ state.ground
                , state.players
                    |> List.map
                        (\player ->
                            { left = player.position |> add (vec2 (-10) (-player.height))
                            , right = player.position |> add (vec2 10 (-player.height))
                            }
                        )
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


execute : Cmd -> State -> (State, Cmd.Cmd Cmd)
execute cmd state =
    case cmd of
        NoOp ->
            state ! []

        Jump ->
            let
                players =
                    state.players
                        |> List.indexedMap
                            (\i player ->
                                if i == state.activePlayer then
                                    { player | velocity = -0.3 }
                                else
                                    player
                            )
            in
                { state | players = players } ! []

        Move direction ->
            { state | movement = Just direction } ! []

        Halt ->
            { state | movement = Nothing } ! []

        Cycle ->
            let
                activePlayer =
                    state.activePlayer
                        + 1
                        |> flip (%) (List.length state.players)
            in
                { state
                    | activePlayer = activePlayer
                } ! []

        Load fn ->
            let
                parse1 xs =
                    case xs of
                        (x::y::xs_) ->
                            vec2 (10*x) (-10*y + 500) :: parse1 xs_
                        _ ->
                            []

                parse2 xs =
                    case xs of
                        (u::v::xs_) ->
                            { left = u, right = v } :: parse2 (v::xs_)
                        _ ->
                            []

                parse =
                    List.map parse1 >> List.map parse2 >> List.concat

                decodeLevel =
                  Json.list (Json.list (Json.float)) |> Json.map parse
            in
                state !  [ Http.get fn decodeLevel |> Http.send LoadOk ]

        LoadOk (Ok ground) ->
            { state | ground = ground
            } ! []

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
    let
        pure x =
            Task.perform (always x) (Task.succeed ())
    in
        (defaultState, Load "level0.json" |> pure)


main : Program Never (Engine.Model State Cmd) (Engine.Msg Cmd)
main =
    Engine.engine jump
