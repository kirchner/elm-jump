module Player exposing (..)

import Dict exposing (Dict)
import Math.Vector2 exposing (..)
import Time exposing (Time)


{-| State of one player entity.  Position is given wrt the center.
Rotation tells how much radians it is rotated counterclockwise.
-}
type alias Player =
    { positions :
        { a : Vec2
        , b : Vec2
        , c : Vec2
        , d : Vec2
        }
    , width : Float
    , height : Float
    , color : String
    , active : Bool
    }


defaultPlayer : Player
defaultPlayer =
    { positions =
        { a = vec2 0 0
        , b = vec2 0 10
        , c = vec2 10 10
        , d = vec2 10 0
        }
    , width = 10
    , height = 10
    , color = "#000"
    , active = False
    }


stepPlayer : Time -> List LineSegment -> Player -> Player -> Player
stepPlayer dt lineSegments curPlayer prevPlayer =
    let
        propagatedPlayer =
            velvetPlayer dt curPlayer prevPlayer

        newPlayer =
            constraintPlayer lineSegments propagatedPlayer curPlayer
    in
        newPlayer


velvetPlayer : Time -> Player -> Player -> Player
velvetPlayer dt curPlayer prevPlayer =
    let
        -- we only consider gravitational acceleration for the moment
        acceleration =
            vec2 0 0.0005

        newPositions =
            { a = velvetPosition .a
            , b = velvetPosition .b
            , c = velvetPosition .c
            , d = velvetPosition .d
            }

        velvetPosition accessor =
            sub (scale 2 (accessor curPlayer.positions)) (accessor prevPlayer.positions)
                |> add (scale (dt * dt) acceleration)
    in
        { curPlayer | positions = newPositions }


constraintPlayer : List LineSegment -> Player -> Player -> Player
constraintPlayer lineSegments newPlayer curPlayer =
    let
        collidedPoints =
            { a = collidedPoint .a
            , b = collidedPoint .b
            , c = collidedPoint .c
            , d = collidedPoint .d
            }

        collidedPoint accessor =
            constraintCollision
                lineSegments
                (accessor newPlayer.positions)
                (accessor curPlayer.positions)

        ( a1, b1 ) =
            constraintDistance
                newPlayer.height
                ( collidedPoints.a, collidedPoints.b )

        ( b2, c1 ) =
            constraintDistance
                newPlayer.width
                ( b1, collidedPoints.c )

        ( c2, d1 ) =
            constraintDistance
                newPlayer.height
                ( c1, collidedPoints.d )

        ( d2, a2 ) =
            constraintDistance
                newPlayer.width
                ( d1, a1 )
    in
        { newPlayer
            | positions =
                { a = a2
                , b = b2
                , c = c2
                , d = d2
                }
        }


constraintCollision : List LineSegment -> Vec2 -> Vec2 -> Vec2
constraintCollision lineSegments newPosition curPosition =
    let
        oldTranslation =
            sub newPosition curPosition

        oldTranslationLineSegment =
            { a = curPosition
            , b = newPosition
            }

        actualTranslation =
            Maybe.withDefault oldTranslation <|
                List.head <|
                    List.sortBy length <|
                        List.map cropTranslation lineSegments

        cropTranslation lineSegment =
            case intersection oldTranslationLineSegment lineSegment of
                Just i ->
                    sub i curPosition

                Nothing ->
                    oldTranslation
    in
        add curPosition actualTranslation


constraintDistance : Float -> ( Vec2, Vec2 ) -> ( Vec2, Vec2 )
constraintDistance wantedDistance ( v, w ) =
    let
        delta =
            sub w v

        actualDistance =
            length delta

        vNew =
            scale ((actualDistance - wantedDistance) / (2 * actualDistance)) delta
                |> add v

        wNew =
            scale ((actualDistance - wantedDistance) / (2 * actualDistance)) delta
                |> sub w
    in
        ( vNew, wNew )


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
                    crossProduct d w / crossProduct v w

                t =
                    crossProduct d v / crossProduct v w

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
                    crossProduct d w / crossProduct v w

                t =
                    crossProduct d v / crossProduct v w

                between a b z =
                    (z >= a) && (z <= b)
            in
                if (between 0 1 s) && (between 0 1 t) then
                    Just (add l1.a (scale s v))
                else
                    Nothing


crossProduct : Vec2 -> Vec2 -> Float
crossProduct v w =
    (getX v) * (getY w) - (getX w) * (getY v)
