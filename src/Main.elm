module Main exposing (..)

-- external

import Engine
import Keyboard
import Math.Vector2 exposing (..)
import Time exposing (Time)


-- internal

import Draw exposing (draw)
import Execute exposing (..)
import Helpers exposing (..)
import Player exposing (Player)
import State exposing (..)


-- MAIN


type alias Recursia =
    Engine.GameLogic State Action


recursia : Recursia
recursia =
    { defaultState = defaultState
    , execute = execute
    , bind = bind
    , step = step
    , init = init
    , draw = draw
    }


main : Program Never (Engine.Model State Action) (Engine.Msg Action)
main =
    Engine.engine recursia



-- STEP


step : Time -> State -> State
step dt state =
    { state
        | player =
            Player.step dt
                state.level.ground
                state.player
    }



-- KEYBINDING


bind : Keyboard.KeyCode -> Bool -> Action
bind keycode pressed =
    if pressed then
        case keycode of
            -- spacebar
            32 ->
                ChargeJump

            -- left
            37 ->
                Move Left

            -- right
            39 ->
                Move Right

            _ ->
                NoOp
    else
        case keycode of
            -- spacebar
            32 ->
                Jump

            -- left
            37 ->
                Stop

            -- right
            39 ->
                Stop

            _ ->
                NoOp
