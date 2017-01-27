module Helpers exposing (..)

import Math.Vector2 exposing (..)


type alias Box =
    { upperLeft : Vec2
    , lowerRight : Vec2
    }


type Direction
    = Left
    | Right


cropTo : Float -> Float -> Float -> Float
cropTo a b t =
    if t <= a then
        a
    else if t >= b then
        b
    else
        t
