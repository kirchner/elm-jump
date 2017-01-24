module Main exposing (..)

import Engine
import Html exposing (Html)
import Keyboard
import Platform.Cmd as Cmd
import Time exposing (Time)


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


type Action
    = NoOp


main : Program Never (Engine.Model State Action) (Engine.Msg Action)
main =
    Engine.engine recursia



-- STATE


type alias State =
    {}


defaultState : State
defaultState =
    {}


init : ( State, Cmd Action )
init =
    ( defaultState, Cmd.none )



-- STEP


step : Time -> State -> State
step dt state =
    state



-- EXECUTE


execute : Action -> State -> ( State, Cmd Action )
execute action state =
    state ! []



-- KEYBINDING


bind : Keyboard.KeyCode -> Bool -> Action
bind keycode pressed =
    NoOp



-- DRAW


draw : Time -> State -> Html Action
draw dt state =
    Html.div [] []
