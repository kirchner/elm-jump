module Main exposing (..)

import Html exposing (..)
import Math.Vector2 exposing (..)
import Player exposing (..)


l1 =
    { a = vec2 0 0
    , b = vec2 50 0
    }


l2 =
    { a = vec2 10 -10
    , b = vec2 10 10
    }


main =
    Html.text <| toString (intersection l1 l2)
