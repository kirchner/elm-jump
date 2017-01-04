module Player exposing (..)

import Math.Vector2 exposing (Vec2, vec2)

type alias Player =
    { position : Vec2
    , velocity : Float
    , color : String
    , active : Bool
    , height : Float
    }


defaultPlayer : Player
defaultPlayer =
    { position = vec2 0 0
    , velocity = 0
    , color = "#000"
    , active = False
    , height = 20
    }
