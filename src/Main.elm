module Main exposing (..)

import Html exposing (Html)
import Html.Events as Events
import Task
import Time exposing (Time)


-- MODEL


type alias Model =
    { state : Maybe ( State, Time )
    , time : Time
    , paused : Bool
    }


defaultModel : Model
defaultModel =
    { state = Nothing
    , time = Time.inMilliseconds 0
    , paused = True
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



-- MSG


type Msg
    = NoOp
    | Tick Time
    | InitState Time
    | UnPause Time
    | Reset
    | Run
    | Pause



-- INIT


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Task.perform InitState Time.now )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Tick newTime ->
            case model.state of
                Just ( state, time ) ->
                    let
                        dt =
                            newTime - time

                        newState =
                            if model.paused then
                                state
                            else
                                step dt state
                    in
                        { model
                            | time = newTime
                            , state = Just ( newState, newTime )
                        }
                            ! []

                Nothing ->
                    ( model, Cmd.none )

        InitState time ->
            { model
                | state = Just ( defaultState, time )
                , paused = True
            }
                ! []

        UnPause currentTime ->
            case model.state of
                Just ( state, time ) ->
                    { model
                        | state = Just ( state, currentTime )
                    }
                        ! []

                Nothing ->
                    ( model, Cmd.none )

        Reset ->
            ( model
            , Task.perform InitState Time.now
            )

        Run ->
            ( { model | paused = False }
            , Task.perform UnPause Time.now
            )

        Pause ->
            { model | paused = True } ! []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        tick =
            if model.paused then
                Sub.none
            else
                Time.every (100 * Time.millisecond) Tick
    in
        Sub.batch [ tick ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        render =
            case model.state of
                Just ( state, time ) ->
                    [ Html.div [] [ Html.text <| toString state.counter ]
                    , Html.div [] [ Html.text <| toString time ]
                    ]

                Nothing ->
                    []
    in
        Html.div [] <|
            List.concat
                [ [ Html.button
                        [ Events.onClick Reset ]
                        [ Html.text "reset" ]
                  , Html.button
                        [ Events.onClick Run ]
                        [ Html.text "run" ]
                  , Html.button
                        [ Events.onClick Pause ]
                        [ Html.text "pause" ]
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
