module Player exposing (..)

import Dict exposing (Dict)
import Math.Vector2 exposing (..)


{-| State of one player entity.  Position is given wrt the center.
Rotation tells how much radians it is rotated counterclockwise.
-}
type alias Player =
    { position : Vec2
    , width : Float
    , height : Float
    , rotation : Float
    , velocity : Float
    , color : String
    , active : Bool
    , resting : Dict Int LineSegment
    }


defaultPlayer : Player
defaultPlayer =
    { position = vec2 0 0
    , width = 40
    , height = 20
    , rotation = 0
    , velocity = 0
    , color = "#000"
    , active = False
    , resting = Dict.empty
    }


{-| Type to select the corner of a Player.  LowerLeft, ..., UpperRight
are with respect to the current rotation.  A, ..., D are always the same
corners.  If rotation == 0, we have A = LowerLeft, B = LowerRight,
C = UpperRight, D == UpperLeft.  (counterclockwise)
-}
type Corner
    = LowerLeft
    | LowerRight
    | UpperRight
    | UpperLeft
    | A
    | B
    | C
    | D


computePosition : Corner -> Player -> Vec2
computePosition corner player =
    let
        x =
            player.width / 2

        y =
            player.height / 2

        permutation =
            (floor (2 * player.rotation / pi + 1 / 2)) % 4

        rotateAndShift dx dy =
            add
                (rotate player.rotation (vec2 dx dy))
                player.position

        ( a, b, c, d ) =
            ( rotateAndShift -x y
            , rotateAndShift x y
            , rotateAndShift x -y
            , rotateAndShift -x -y
            )

        ( ll, lr, ur, ul ) =
            case permutation of
                0 ->
                    ( a, b, c, d )

                1 ->
                    ( d, a, b, c )

                2 ->
                    ( c, d, a, b )

                3 ->
                    ( b, c, d, a )

                _ ->
                    Debug.crash "this permutation is not possible"
    in
        case corner of
            A ->
                a

            B ->
                b

            C ->
                c

            D ->
                d

            LowerLeft ->
                ll

            LowerRight ->
                lr

            UpperRight ->
                ur

            UpperLeft ->
                ul


{-| Rotate the vector counterclockwise.
-}
rotate : Float -> Vec2 -> Vec2
rotate angle v =
    let
        ( x, y ) =
            toTuple v
    in
        vec2
            (cos angle * x + sin angle * y)
            (-1 * sin angle * x + cos angle * y)


type alias LineSegment =
    { a : Vec2
    , b : Vec2
    }


type alias InfiniteLine =
    { anchor : Vec2
    , direction : Vec2
    }


intersectionInfiniteLineLineSegment : InfiniteLine -> LineSegment -> Maybe Vec2
intersectionInfiniteLineLineSegment infiniteLine lineSegment =
    let
        v =
            infiniteLine.direction

        w =
            sub lineSegment.b lineSegment.a

        d =
            sub lineSegment.a infiniteLine.anchor

        n =
            crossProduct v w
    in
        if n == 0 then
            Nothing
        else
            let
                s =
                    Debug.log "s" <|
                        crossProduct d v
                            / crossProduct v w

                t =
                    Debug.log "t" <|
                        crossProduct d w
                            / crossProduct v w

                between a b z =
                    (z > a) && (z < b)
            in
                if (t |> between 0 1) then
                    Just (add infiniteLine.anchor (scale s v))
                else
                    Nothing


intersection : LineSegment -> LineSegment -> Maybe Vec2
intersection l1 l2 =
    let
        v =
            sub l1.b l1.a

        w =
            sub l2.b l2.a

        d =
            sub l2.a l1.a

        n =
            crossProduct v w
    in
        if n == 0 then
            Nothing
        else
            let
                s =
                    crossProduct d v / crossProduct v w

                t =
                    crossProduct d w / crossProduct v w

                between a b z =
                    (z > a) && (z < b)
            in
                if (s |> between 0 1) && (t |> between 0 1) then
                    Just (add l1.a (scale s v))
                else
                    Nothing


crossProduct : Vec2 -> Vec2 -> Float
crossProduct v w =
    (getX v) * (getY w) - (getX w) * (getY v)
