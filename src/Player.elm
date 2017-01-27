module Player exposing (..)

-- external

import Time exposing (Time)
import Math.Vector2 exposing (..)


-- internal

import Helpers exposing (..)


{-| The players bounding box is 40x40.  The Position is the lower middle
point.
-}
type alias Player =
    { position : Vec2
    , velocity : Vec2
    , move : Maybe ( Direction, Float )
    }


default =
    { position = vec2 220 200
    , velocity = vec2 0 0
    , move = Nothing
    }


move : Maybe Direction -> Player -> Player
move direction player =
    case direction of
        Just Left ->
            case player.move of
                Just ( Left, _ ) ->
                    player

                _ ->
                    { player | move = Just ( Left, 0 ) }

        Just Right ->
            case player.move of
                Just ( Right, _ ) ->
                    player

                _ ->
                    { player | move = Just ( Right, 0 ) }

        Nothing ->
            { player | move = Nothing }



-- STEP


step : Time -> List Box -> Player -> Player
step dt ground player =
    let
        velocity =
            case player.move of
                Just ( Left, duration ) ->
                    -0.3 * cropTo 0 1 (duration / 400)

                Just ( Right, duration ) ->
                    0.3 * cropTo 0 1 (duration / 400)

                Nothing ->
                    0

        propagatedPlayer =
            propagate dt
                { player
                    | velocity = vec2 velocity (getY player.velocity)
                    , move =
                        Maybe.map (\( dir, dur ) -> ( dir, dur + dt )) player.move
                }

        actualPlayer =
            constraint ground propagatedPlayer player
    in
        actualPlayer


propagate : Time -> Player -> Player
propagate dt player =
    let
        acceleration =
            vec2 0 0.001

        newVelocity =
            add player.velocity (scale dt acceleration)

        newPosition =
            add player.position (scale dt newVelocity)
    in
        { player
            | position = newPosition
            , velocity = newVelocity
        }


constraint : List Box -> Player -> Player -> Player
constraint ground newPlayer curPlayer =
    constraintBoundingBox <|
        List.foldr constraintBox (constraintBoundingBox newPlayer) ground


constraintBoundingBox : Player -> Player
constraintBoundingBox player =
    let
        ( x, y ) =
            toTuple player.position

        newX =
            cropTo 20 620 x

        newY =
            cropTo 40 640 y

        newVelocity =
            if newY == 640 then
                vec2 (getX player.velocity) 0
            else
                player.velocity
    in
        { player
            | position = vec2 newX newY
            , velocity = newVelocity
        }


constraintBox : Box -> Player -> Player
constraintBox box player =
    let
        ( x, y ) =
            toTuple player.position

        ( playerCenterX, playerCenterY ) =
            ( x, y - 20 )

        ( playerWidth, playerHeight ) =
            ( 40, 40 )

        ( qx, qy ) =
            toTuple box.upperLeft

        ( px, py ) =
            toTuple box.lowerRight

        ( boxCenterX, boxCenterY ) =
            ( (qx + px) / 2, (qy + py) / 2 )

        ( boxWidth, boxHeight ) =
            ( px - qx, py - qy )

        xOverlap =
            abs (playerCenterX - boxCenterX) - (playerWidth + boxWidth) / 2

        yOverlap =
            abs (playerCenterY - boxCenterY) - (playerHeight + boxHeight) / 2
    in
        if (xOverlap < 0) && (yOverlap < 0) then
            -- box and player intersect
            if xOverlap >= yOverlap then
                if playerCenterX <= boxCenterX then
                    -- move to the left
                    { player
                        | position =
                            sub player.position (vec2 xOverlap 0)
                    }
                else
                    -- move to the right
                    { player
                        | position =
                            sub player.position (vec2 xOverlap 0)
                    }
            else if playerCenterY <= boxCenterY then
                -- move up
                { player
                    | position =
                        add player.position (vec2 0 yOverlap)
                    , velocity =
                        vec2 (getX player.velocity) 0
                }
            else
                -- move down
                { player
                    | position =
                        sub player.position (vec2 0 yOverlap)
                    , velocity =
                        vec2 (getX player.velocity) 0
                }
        else
            -- box and player do not intersect
            player
