module Camera exposing
    (..
    )


import Math.Vector2 as Vec2 exposing (Vec2, vec2, getX, getY)
import Math.Matrix4 as Mat4 exposing (Mat4, mul, makeScale3, makeTranslate3)
import Time exposing (Time)


type alias Camera =
    { position : Vec2
    , scale : Float
    }


defaultCamera : Camera
defaultCamera =
    { position = vec2 200 (-50)
    , scale = 1
    }


-- T = Transitionable Camera

type alias T =
  { lastCamera : Camera
  , lastFrame : Time
  , target : Maybe (Time, Camera)
  , duration : Time
  }


defaultT : T
defaultT =
  { lastCamera = defaultCamera
  , lastFrame = 0
  , target = Nothing
  , duration = 750*Time.millisecond
  }


step : Time -> T -> T
step currentTime tcamera =
    case tcamera.target of

      Just (futureTime, futureCamera) ->
          let
              t = 
                1 - clamp 0 1 (dt / tcamera.duration)

              dt =
                futureTime - currentTime

              lastCamera =
                lerp t tcamera.lastCamera futureCamera
          in
              if t == 1.0 then
                      { tcamera
                          | lastCamera = futureCamera
                          , lastFrame = currentTime
                          , target = Nothing
                      }
                  else
                      { tcamera
                          | lastCamera = lastCamera
                          , lastFrame = currentTime
                      }
      Nothing ->

          { tcamera
              | lastFrame = currentTime
          }


lerp : Time -> Camera -> Camera -> Camera
lerp t currentCamera futureCamera =
  let
      lerpv2 t u v =
        vec2 ((1-t) * (getX u) + t * (getX v))
             ((1-t) * (getY u) + t * (getY v))

      lerpf t x y =
        (1-t) * x + t * y
  in
    { position =
        lerpv2 t currentCamera.position futureCamera.position

    , scale =
        lerpf t currentCamera.scale futureCamera.scale
    }


retarget : Time -> Camera -> T -> T
retarget currentTime camera tcamera =
    { tcamera
        | target = Just (currentTime + tcamera.duration, camera)
    }


transform : Camera -> String
transform camera =
    let
        width = 600
        height = 400

        halfWidth = width / 2
        halfHeight = height / 2
    in
        [ ""
        
        , "translate("
        , halfWidth |> toString
        , " "
        , halfHeight |> toString
        , ") "

        , "scale("
        , camera.scale |> toString
        , ") "

        , "translate("
        , -1*(camera.position |> getX) |> toString
        , " "
        , -1*((camera.position |> getY)) |> toString
        , ") "

        ]
    |> String.join ""
