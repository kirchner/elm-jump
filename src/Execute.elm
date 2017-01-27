module Execute exposing (..)

-- external

import Math.Vector2 exposing (..)


-- internal

import Helpers exposing (..)
import Player
import State exposing (..)


execute : Action -> State -> ( State, Cmd Action )
execute action state =
    case action of
        NoOp ->
            state ! []

        ChargeJump ->
            { state | player = Player.chargeJump state.player } ! []

        Jump ->
            { state | player = Player.jump state.player } ! []

        Move newDirection ->
            { state | player = Player.move (Just newDirection) state.player } ! []

        Stop ->
            { state | player = Player.move Nothing state.player } ! []
