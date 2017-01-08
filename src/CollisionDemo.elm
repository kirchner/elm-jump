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
            , rotation = -1 * pi / 4
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


{-| Translate and rotate the player in the given direction.
-}
movementOfPlayer : Time -> Maybe Direction -> Player -> Player
movementOfPlayer dt movement player =
    let
        speed =
            0.1

        rotationSpeed =
            0.001

        directionVector =
            case movement of
                Just Left ->
                    vec2 -1 0

                Just Right ->
                    vec2 1 0

                Nothing ->
                    vec2 0 0

        translation =
            directionVector
                |> scale (speed * (Time.inMilliseconds dt))

        actualTranslation =
            Maybe.withDefault translation <|
                List.head <|
                    Debug.log "ascension" <|
                        List.sortWith compareY <|
                            List.map (adjustTranslation translation) <|
                                Dict.toList player.resting

        compareY v w =
            if (getY v) < (getY w) then
                LT
            else if (getY v) > (getY w) then
                GT
            else
                EQ

        adjustTranslation translation ( cornerIndex, lineSegment ) =
            let
                _ =
                    Debug.log "adjusted translation"

                corner =
                    case cornerIndex of
                        0 ->
                            A

                        1 ->
                            B

                        2 ->
                            C

                        3 ->
                            D

                        _ ->
                            A

                cornerPosition =
                    computePosition corner player

                newTranslation =
                    case
                        intersectionInfiniteLineLineSegment
                            { anchor = add cornerPosition translation
                            , direction = vec2 0 1
                            }
                            lineSegment
                    of
                        Just intersection ->
                            sub intersection cornerPosition

                        Nothing ->
                            translation
            in
                newTranslation

        rotation =
            if Dict.isEmpty player.resting then
                case movement of
                    Just Left ->
                        (Time.inMilliseconds dt) * rotationSpeed

                    Just Right ->
                        -1 * (Time.inMilliseconds dt) * rotationSpeed

                    Nothing ->
                        0
            else
                0
    in
        { player
            | position = add player.position actualTranslation
            , rotation = player.rotation + rotation
        }


round : Vec2 -> Vec2
round v =
    vec2 (toFloat <| ceiling <| getX v) (toFloat <| ceiling <| getY v)


{-| Apply gravitational force onto player.
-}
gravityOfPlayer : Time -> Player -> Player
gravityOfPlayer dt player =
    let
        acceleration =
            0.0004

        newVelocity =
            if Dict.isEmpty player.resting then
                acceleration * (Time.inMilliseconds dt) + player.velocity
            else
                player.velocity

        newPosition =
            if Dict.isEmpty player.resting then
                vec2 0 1
                    |> scale (newVelocity * (Time.inMilliseconds dt))
                    |> add player.position
            else
                player.position
    in
        { player
            | position = newPosition
            , velocity = newVelocity
        }


{-| Adjust the translation of the player taking into account collision
of its corners with the lines.  Returns the decreased translation vector
and the resting corner.

TODO: also take into account the additional rotation
-}
collisionOfPlayer : Player -> Player -> List Line -> Player
collisionOfPlayer oldPlayer newPlayer lines =
    let
        oldTranslation =
            sub newPlayer.position oldPlayer.position

        cropTranslationOfCorner corner line ( translation, restingLineSegment ) =
            let
                cornerPosition =
                    computePosition corner oldPlayer

                translationLineSegment =
                    { a = cornerPosition
                    , b = add cornerPosition translation
                    }

                lineSegment line =
                    { a = line.left
                    , b = line.right
                    }
            in
                case intersection translationLineSegment (lineSegment line) of
                    Just i ->
                        ( sub i cornerPosition, Just (lineSegment line) )

                    Nothing ->
                        ( translation, restingLineSegment )

        newTranslationOfCorner corner =
            List.foldr (cropTranslationOfCorner corner) ( oldTranslation, Nothing ) lines

        ( smallestTranslation, possibleResting ) =
            Maybe.withDefault ( vec2 0 0, Nothing ) <|
                List.head <|
                    let
                        format ( ( translation, restingLineSegment ), corner ) =
                            case restingLineSegment of
                                Just lineSegment ->
                                    ( translation, Just ( lineSegment, corner ) )

                                Nothing ->
                                    ( translation, Nothing )
                    in
                        List.map format <|
                            List.sortBy
                                (length << Tuple.first << Tuple.first)
                                [ ( newTranslationOfCorner A, 0 )
                                , ( newTranslationOfCorner B, 1 )
                                , ( newTranslationOfCorner C, 2 )
                                , ( newTranslationOfCorner D, 3 )
                                ]

        newResting =
            case possibleResting of
                Just ( lineSegment, corner ) ->
                    Dict.insert corner lineSegment newPlayer.resting

                Nothing ->
                    newPlayer.resting

        newVelocity =
            case possibleResting of
                Just _ ->
                    0

                Nothing ->
                    newPlayer.velocity
    in
        { newPlayer
            | position = add oldPlayer.position smallestTranslation
            , resting = newResting
            , velocity = newVelocity
        }


stepPlayers : Time -> State -> State
stepPlayers dt state =
    let
        newPlayers =
            state.players
                |> List.map (stepPlayer dt state)
    in
        { state
            | players = newPlayers
        }


stepPlayer : Time -> State -> Player -> Player
stepPlayer dt state player =
    let
        newPlayer =
            gravityOfPlayer dt <| movementOfPlayer dt state.movement player

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
        collisionOfPlayer player newPlayer lines


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
                                    { player
                                        | velocity = -0.3
                                        , resting = Dict.empty
                                    }
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
