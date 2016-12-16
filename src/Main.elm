module Main exposing (..)

import AnimationFrame
import Html exposing (Html)
import Html.Events as Events
import Svg
import Svg.Attributes as Svg
import Task
import Time exposing (Time)


-- MODEL


type alias Model =
    { state : State
    , stateTime : Time
    , currentTime : Time
    , paused : Bool
    }


defaultModel : Model
defaultModel =
    { state = defaultState
    , stateTime = 0 * Time.millisecond
    , paused = True
    , currentTime = 0 * Time.millisecond
    }



-- STATE


type alias State =
    { counter : Int }


defaultState : State
defaultState =
    { counter = 0 }



-- STEP


step : Time -> State -> State
step dt state =
    { state
        | counter =
            state.counter + (round <| Time.inMilliseconds dt)
    }



-- DRAW


draw : Time -> State -> Html m
draw dt state =
    let
        newState =
            step dt state

        offset =
            sin <| (toFloat newState.counter / 1000)
    in
        Svg.svg
            [ Svg.width "600"
            , Svg.height "400"
            , Svg.viewBox "0 0 600 400"
            ]
            [ Svg.circle
                [ Svg.cx "300"
                , Svg.cy "200"
                , Svg.r <| toString <| 55 + 50 * offset
                , Svg.fill "green"
                ]
                []
            ]



-- MSG


type Msg
    = NoOp
    | Init Time
    | Pause Time
    | UnPause Time
    | Tick Time
    | Reset
    | Run
    | Stop
    | Redraw Time



-- INIT


init : ( Model, Cmd Msg )
init =
    ( defaultModel
    , Cmd.batch
        [ Task.perform Init Time.now
        , Task.perform Redraw Time.now
        ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Init currentTime ->
            update (Redraw currentTime)
                { model
                    | state = defaultState
                    , stateTime = currentTime
                    , paused = True
                }

        Pause currentTime ->
            if model.paused then
                model ! []
            else
                let
                    newState =
                        step (currentTime - model.stateTime) model.state
                in
                    update (Redraw currentTime)
                        { model
                            | state = newState
                            , stateTime = currentTime
                            , paused = True
                        }

        UnPause currentTime ->
            if model.paused then
                update (Redraw currentTime)
                    { model
                        | stateTime = currentTime
                        , paused = False
                    }
            else
                model ! []

        Tick currentTime ->
            if model.paused then
                model ! []
            else
                let
                    newState =
                        step (currentTime - model.stateTime) model.state
                in
                    update (Redraw currentTime)
                        { model
                            | state = newState
                            , stateTime = currentTime
                        }

        Reset ->
            ( model, Task.perform Init Time.now )

        Run ->
            ( model, Task.perform UnPause Time.now )

        Stop ->
            ( model, Task.perform Pause Time.now )

        Redraw currentTime ->
            { model | currentTime = currentTime } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        tick =
            if model.paused then
                Sub.none
            else
                Time.every (2000 * Time.millisecond) Tick

        redraw =
            if model.paused then
                Sub.none
            else
                AnimationFrame.times Redraw
    in
        Sub.batch [ tick, redraw ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        render =
            [ draw (model.currentTime - model.stateTime) model.state ]
    in
        Html.div [] <|
            List.concat
                [ [ Html.button
                        [ Events.onClick Reset ]
                        [ Html.text "reset" ]
                  , Html.button
                        [ Events.onClick Stop ]
                        [ Html.text "stop" ]
                  , Html.button
                        [ Events.onClick Run ]
                        [ Html.text "run" ]
                  , Html.div [] [ Html.text <| "current time: " ++ (toString model.currentTime) ]
                  , Html.div [] [ Html.text <| "state time: " ++ (toString model.stateTime) ]
                  ]
                , render
                ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
