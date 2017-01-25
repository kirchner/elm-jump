module Engine exposing (..)

import AnimationFrame
import Html.Events as Events
import Html exposing (Html)
import Keyboard
import Mouse
import Task
import Time exposing (Time)
import Math.Vector2 exposing (Vec2, vec2)


-- MODEL


type alias Model state action =
    { gameLogic : GameLogic state action
    , state : state
    , stateTime : Time
    , currentTime : Time
    , paused : Bool
    , cursor : Vec2
    }


defaultModel : GameLogic state action -> state -> Model state action
defaultModel gameLogic initState =
    { gameLogic = gameLogic
    , state = initState
    , stateTime = 0 * Time.millisecond
    , paused = True
    , currentTime = 0 * Time.millisecond
    , cursor = vec2 0 0
    }



-- GAMELOGIC


type alias GameLogic state action =
    { defaultState : state
    , execute : action -> state -> ( state, Cmd action )
    , bind : Keyboard.KeyCode -> Bool -> action
    , step : Time -> state -> state
    , draw : Time -> state -> Html action
    , init : ( state, Cmd action )
    }



-- MSG


type Msg action
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
    | Mouse Mouse.Position
    | Async action


trivialLift : action -> Msg action
trivialLift msg =
    NoOp



-- INIT


init : GameLogic state action -> ( Model state action, Cmd (Msg action) )
init gameLogic =
    let
        ( initState, initAction ) =
            gameLogic.init
    in
        ( defaultModel gameLogic initState
        , Cmd.batch
            [ Task.perform Init Time.now
            , Task.perform Redraw Time.now
            , initAction |> Cmd.map Async
            ]
        )



-- UPDATE


update : Msg action -> Model state action -> ( Model state action, Cmd (Msg action) )
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
                action =
                    model.gameLogic.bind keycode True

                ( newState, newAction ) =
                    model.gameLogic.execute action model.state
            in
                { model | state = newState } ! [ newAction |> Cmd.map Async ]

        KeyUp keycode ->
            let
                action =
                    model.gameLogic.bind keycode False

                ( newState, newAction ) =
                    model.gameLogic.execute action model.state
            in
                { model | state = newState } ! [ newAction |> Cmd.map Async ]

        Mouse position ->
            let
                cursor =
                    vec2 (position.x |> toFloat) (position.y |> toFloat)
            in
                { model | cursor = cursor } ! []

        Async action ->
            let
                ( newState, newAction ) =
                    model.gameLogic.execute action model.state
            in
                { model | state = newState } ! [ newAction |> Cmd.map Async ]



-- SUBSCRIPTIONS


subscriptions : Model state action -> Sub (Msg action)
subscriptions model =
    let
        redraw =
            if model.paused then
                Sub.none
            else
                AnimationFrame.times Tick

        keyDowns =
            Keyboard.downs KeyDown

        keyUps =
            Keyboard.ups KeyUp

        mouse =
            Mouse.moves Mouse
    in
        Sub.batch
            [ redraw
            , keyDowns
            , keyUps
            , mouse
            ]



-- VIEW


view : Model state action -> Html (Msg action)
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
                  , Html.div [] [ Html.text <| "mouse position: " ++ toString model.cursor ]
                  ]
                , [ Html.map Async render ]
                ]


engine : GameLogic state action -> Program Never (Model state action) (Msg action)
engine gameLogic =
    Html.program
        { init = init gameLogic
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
