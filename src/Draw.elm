module Draw exposing (..)

-- external

import Html exposing (Html)
import Math.Vector2 exposing (..)
import Svg exposing (Svg)
import Svg.Attributes as Svg
import Time exposing (Time)


-- internal

import Helpers exposing (..)
import Player exposing (Player)
import State exposing (..)


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
