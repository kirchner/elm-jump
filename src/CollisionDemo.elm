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
    , players : List Player
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
    , players =
        [ { defaultPlayer
            | position = vec2 300 0
            , height = 20
            , rotation = -1 * pi / 5
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
                        if List.isEmpty player.resting then
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
                        else
                            let
                                newVelocity =
                                    0

                                newPosition =
                                    player.position
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

        rotationSpeed =
            0.001

        direction =
            case state.movement of
                Just Left ->
                    -1

                Just Right ->
                    1

                Nothing ->
                    0

        directionVector =
            vec2 direction 0

        players =
            state.players
                |> List.indexedMap
                    (\i player ->
                        if i == state.activePlayer then
                            let
                                newPosition =
                                    directionVector
                                        |> scale (speed * (Time.inMilliseconds dt))
                                        |> add player.position

                                newRotation =
                                    -- TODO: check if player is falling
                                    if player.velocity > 0 then
                                        player.rotation - direction * (Time.inMilliseconds dt) * rotationSpeed
                                    else
                                        player.rotation
                            in
                                { player
                                    | position = newPosition
                                    , rotation = newRotation
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
                            translation =
                                sub newPlayer.position oldPlayer.position

                            ( actualTranslation, newRestingCorner ) =
                                collisionOfPlayer oldPlayer translation lines

                            newPosition =
                                add oldPlayer.position actualTranslation

                            newResting =
                                case newRestingCorner of
                                    Just corner ->
                                        corner :: newPlayer.resting

                                    Nothing ->
                                        newPlayer.resting
                        in
                            { newPlayer
                                | position = newPosition
                                , resting = newResting
                            }
                    )
    in
        { newState | players = players }


{-| Adjust the translation of the player taking into account collision
of its corners with the lines.  Returns the decreased translation vector
and the resting corner.
-}
collisionOfPlayer : Player -> Vec2 -> List Line -> ( Vec2, Maybe Corner )
collisionOfPlayer player oldTranslation lines =
    let
        translationUntilLine corner line translation =
            let
                cornerPosition =
                    computePosition corner player

                translationLine =
                    { a = cornerPosition
                    , b = add cornerPosition translation
                    }

                lineSegment line =
                    { a = line.left
                    , b = line.right
                    }
            in
                case intersection translationLine (lineSegment line) of
                    Just i ->
                        Debug.log "intersection" <|
                            sub i cornerPosition

                    Nothing ->
                        translation

        newTranslation corner =
            List.foldr (translationUntilLine corner) oldTranslation lines

        ( smallestTranslation, corner ) =
            -- TODO: there is no real default here, since this case
            -- should never happen
            Maybe.withDefault ( oldTranslation, A ) <|
                List.head <|
                    List.sortBy (length << Tuple.first)
                        [ ( newTranslation A, A )
                        , ( newTranslation B, B )
                        , ( newTranslation C, C )
                        , ( newTranslation D, D )
                        ]

        restingCorner =
            if smallestTranslation == oldTranslation then
                Nothing
            else
                Just corner
    in
        ( smallestTranslation, restingCorner )


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
    let
        drawCorner corner attr =
            Svg.circle
                (attr
                    ++ [ Svg.cx <| toString <| getX <| computePosition corner player
                       , Svg.cy <| toString <| getY <| computePosition corner player
                       ]
                )
                []
    in
        [ Svg.rect
            [ Svg.x <| toString (getX player.position - 10)
            , Svg.y <| toString (getY player.position - player.height)
            , Svg.width "20"
            , Svg.height (player.height |> toString)
            , Svg.fill player.color
            ]
            []
        , drawCorner LowerLeft
            [ Svg.fill player.color
            , Svg.r "3"
            ]
        , drawCorner LowerRight
            [ Svg.fill player.color
            , Svg.r "3"
            ]
        , drawCorner A
            [ Svg.fill "green"
            , Svg.r "1"
            ]
        , drawCorner B
            [ Svg.r "1" ]
        , drawCorner C
            [ Svg.r "1" ]
        , drawCorner D
            [ Svg.r "1" ]
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


execute : Cmd -> State -> ( State, Cmd.Cmd Cmd )
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
