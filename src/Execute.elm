module Execute exposing (..)

-- external

import Math.Vector2 exposing (..)


-- internal

import Helpers exposing (..)
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
            let
                delta =
                    case newDirection of
                        Left ->
                            vec2 -0.3 0

                        Right ->
                            vec2 0.3 0

                newPlayer =
                    state.player
                        |> (\player ->
                                case player.move of
                                    Just ( prevDirection, duration ) ->
                                        if prevDirection == newDirection then
                                            player
                                        else
                                            { player
                                                | velocity =
                                                    vec2 (getX delta) (getY player.velocity)
                                                , move = Just ( newDirection, 0 )
                                            }

                                    Nothing ->
                                        { player
                                            | velocity =
                                                vec2 (getX delta) (getY player.velocity)
                                            , move = Just ( newDirection, 0 )
                                        }
                           )
            in
                { state
                    | player = newPlayer
                }
                    ! []

        Stop ->
            let
                newPlayer =
                    state.player
                        |> (\player ->
                                { player
                                    | velocity =
                                        vec2 0 (getY player.velocity)
                                    , move = Nothing
                                }
                           )
            in
                { state
                    | player = newPlayer
                }
                    ! []
