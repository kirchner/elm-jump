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

        Jump ->
            let
                jumpSpeed =
                    if getY state.player.velocity == 0 then
                        -0.5
                    else
                        0

                newPlayer =
                    state.player
                        |> (\player ->
                                { player
                                    | velocity =
                                        add player.velocity (vec2 0 jumpSpeed)
                                }
                           )
            in
                { state
                    | player = newPlayer
                }
                    ! []

        Move newDirection ->
            { state
                | player =
                    Player.move (Just newDirection) state.player
            }
                ! []

        Stop ->
            { state
                | player =
                    Player.move Nothing state.player
            }
                ! []
