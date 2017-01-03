module Engine exposing (..)

import AnimationFrame
import Html exposing (Html)
import Html.Events as Events
import Keyboard
import Task
import Time exposing (Time)


-- MODEL


type alias Model state cmd =
    { gameLogic : GameLogic state cmd
    , state : state
    , stateTime : Time
    , currentTime : Time
    , paused : Bool
    }


defaultModel : GameLogic state cmd -> Model state cmd
defaultModel gameLogic =
    { gameLogic = gameLogic
    , state = gameLogic.defaultState
    , stateTime = 0 * Time.millisecond
    , paused = True
    , currentTime = 0 * Time.millisecond
    }



-- GAMELOGIC


type alias GameLogic state cmd =
    { defaultState : state
    , execute : cmd -> state -> state
    , bind : Keyboard.KeyCode -> Bool -> cmd
    , step : Time -> state -> state
    , draw : Time -> state -> Html cmd
    }



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
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode


trivialLift : cmd -> Msg
trivialLift msg =
    NoOp



-- INIT


init : GameLogic state cmd -> ( Model state cmd, Cmd Msg )
init gameLogic =
    ( defaultModel gameLogic
    , Cmd.batch
        [ Task.perform Init Time.now
        , Task.perform Redraw Time.now
        ]
    )



-- UPDATE


update : Msg -> Model state cmd -> ( Model state cmd, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Init currentTime ->
            update (Redraw currentTime)
                { model
                    | state = model.gameLogic.defaultState
                    , stateTime = currentTime
                    , paused = True
                }

        Pause currentTime ->
            if model.paused then
                model ! []
            else
                let
                    newState =
                        model.gameLogic.step (currentTime - model.stateTime) model.state
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
                        model.gameLogic.step (currentTime - model.stateTime) model.state
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

        KeyDown keycode ->
            let
                cmd =
                    model.gameLogic.bind keycode True

                newState =
                    model.gameLogic.execute cmd model.state
            in
                { model | state = newState } ! []

        KeyUp keycode ->
            let
                cmd =
                    model.gameLogic.bind keycode False

                newState =
                    model.gameLogic.execute cmd model.state
            in
                { model | state = newState } ! []



-- SUBSCRIPTIONS


subscriptions : Model state cmd -> Sub Msg
subscriptions model =
    let
        --        tick =
        --            if model.paused then
        --                Sub.none
        --            else
        --                Time.every (100 * Time.millisecond) Tick
        redraw =
            if model.paused then
                Sub.none
            else
                AnimationFrame.times Tick

        keyDowns =
            Keyboard.downs KeyDown

        keyUps =
            Keyboard.ups KeyUp
    in
        Sub.batch
            --            [ tick
            [ redraw
            , keyDowns
            , keyUps
            ]



-- VIEW


view : Model state cmd -> Html Msg
view model =
    let
        render =
            model.gameLogic.draw (model.currentTime - model.stateTime) model.state
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
                , [ Html.map trivialLift render ]
                ]


engine : GameLogic state cmd -> Program Never (Model state cmd) Msg
engine gameLogic =
    Html.program
        { init = init gameLogic
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
