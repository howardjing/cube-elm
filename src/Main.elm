module Main exposing (main)

import Html exposing (div, button, text)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Time exposing (Time, millisecond)
import Task exposing (Task)
import Keyboard exposing (KeyCode)
import Char exposing (toCode)
import Html.App as App
import MainCss as Styles


main : Program Never
main =
    App.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { start : Maybe Time
    , inspectionTime : Maybe Float
    , solveTime : Maybe Float
    , elapsed : Float
    }


initialModel : Model
initialModel =
    { start = Nothing
    , inspectionTime = Nothing
    , solveTime = Nothing
    , elapsed = 0
    }


type Msg
    = WithTime (Time -> Msg)
    | StartInspection Time
    | StartSolve Time
    | StopSolve Time
    | TickInspection Time
    | TickSolve Time
    | KeyStartInspection KeyCode
    | KeyStartSolve KeyCode
    | KeyStopSolve KeyCode


perform : Msg -> Cmd Msg
perform msg =
    Task.perform identity (\_ -> msg) (Task.succeed Nothing)


startSolve : Model -> Float
startSolve model =
    Maybe.withDefault 0 (Maybe.map2 (+) model.start model.inspectionTime)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartInspection time ->
            ( { initialModel | start = Just time }, Cmd.none )

        StartSolve time ->
            ( { model
                | inspectionTime = Maybe.map (\start -> time - start) model.start
              }
            , perform (TickInspection time)
            )

        StopSolve time ->
            ( { model | solveTime = Just (time - (startSolve model)) }
            , perform (TickSolve time)
            )

        TickInspection time ->
            ( { model | elapsed = time - (Maybe.withDefault 0 model.start) }, Cmd.none )

        TickSolve time ->
            ( { model | elapsed = time - (startSolve model) }, Cmd.none )

        WithTime handleTime ->
            ( model, (Task.perform identity handleTime Time.now) )

        KeyStartInspection code ->
            if code == (toCode ' ') then
                ( model, perform recordStartInspection )
            else
                ( model, Cmd.none )

        KeyStartSolve code ->
            if code == (toCode ' ') then
                ( model, perform recordStartSolve )
            else
                ( model, Cmd.none )

        KeyStopSolve code ->
            if code == (toCode ' ') then
                ( model, perform recordStopSolve )
            else
                ( model, Cmd.none )


recordStartInspection : Msg
recordStartInspection =
    WithTime (\time -> StartInspection time)


recordStartSolve : Msg
recordStartSolve =
    WithTime (\time -> StartSolve time)


recordStopSolve : Msg
recordStopSolve =
    WithTime (\time -> StopSolve time)


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Nothing ->
            True

        _ ->
            False


isInspecting : Model -> Bool
isInspecting model =
    (not (isNothing model.start)) && (isNothing model.inspectionTime)


isSolving : Model -> Bool
isSolving model =
    (not (isNothing model.start))
        && (not (isNothing model.inspectionTime))
        && (isNothing model.solveTime)


subscriptions : Model -> Sub Msg
subscriptions model =
    if isInspecting model then
        Sub.batch
            [ Time.every (100 * millisecond) TickInspection
            , Keyboard.ups KeyStartSolve
            ]
    else if isSolving model then
        Sub.batch
            [ Time.every (100 * millisecond) TickSolve
            , Keyboard.downs KeyStopSolve
            ]
    else
        Sub.batch
            [ Keyboard.downs KeyStartInspection
            ]


{ class } =
    Styles.namespace
view : Model -> Html.Html Msg
view model =
    let
        renderButton =
            if isInspecting model then
                button
                    [ class [ Styles.Button ]
                    , onMouseUp recordStartSolve
                    ]
                    [ text "Release space to start solving" ]
            else if isSolving model then
                button
                    [ class [ Styles.Button ]
                    , onClick recordStopSolve
                    ]
                    [ text "Press space to stop solving" ]
            else
                button
                    [ class [ Styles.Button ]
                    , onMouseDown recordStartInspection
                    ]
                    [ text "Press and hold space to start inspecting" ]

        timer millis =
            div [] [ text (toString millis) ]
    in
        div []
            [ timer model.elapsed
            , renderButton
            , div [] [ text ("inspection time: " ++ (toString model.inspectionTime)) ]
            , div [] [ text ("solve time: " ++ (toString model.solveTime)) ]
            ]
