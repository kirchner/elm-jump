module Helpers exposing (..)

import Math.Vector2 exposing (..)


type alias Box =
    { upperLeft : Vec2
    , lowerRight : Vec2
    }


type Direction
    = Left
    | Right
