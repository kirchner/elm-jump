module State exposing (..)

-- external

import Math.Vector2 exposing (..)


-- internal

import Helpers exposing (..)
import Player exposing (Player)


-- STATE


type alias State =
    { level : Level
    , player : Player
    }


defaultState : State
defaultState =
    --{ levels = Dict.empty
    --, currentLevel = Nothing
    --}
    testState


testState : State
testState =
    { level = testLevel
    , player = Player.default
    }


init : ( State, Cmd Action )
init =
    ( testState, Cmd.none )



-- ACTION


type Action
    = NoOp
    | Jump
    | Move Direction
    | Stop


{-| All Levels are 640x640.
-}
type alias Level =
    { ground : List Box
    }


testLevel : Level
testLevel =
    { ground =
        [ { upperLeft = vec2 0 40
          , lowerRight = vec2 400 80
          }
        , { upperLeft = vec2 320 200
          , lowerRight = vec2 640 240
          }
        , { upperLeft = vec2 0 400
          , lowerRight = vec2 260 640
          }
        , { upperLeft = vec2 260 500
          , lowerRight = vec2 300 640
          }
        , { upperLeft = vec2 300 550
          , lowerRight = vec2 380 640
          }
        ]
    }
