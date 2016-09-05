module Main exposing (main)

import Html exposing (div, button, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Time exposing (Time, millisecond)
import Task exposing (Task)
import Keyboard exposing (KeyCode)
import Char exposing (toCode)
import Html.App as App
import MainCss as Styles
import String exposing (padLeft)


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
            [ Time.every (1 * millisecond) TickInspection
            , Keyboard.ups KeyStartSolve
            ]
    else if isSolving model then
        Sub.batch
            [ Time.every (1 * millisecond) TickSolve
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
                    [ text "Release space when finished inspecting" ]
            else if isSolving model then
                button
                    [ class [ Styles.Button ]
                    , onClick recordStopSolve
                    ]
                    [ text "Press space when finished solving" ]
            else
                button
                    [ class [ Styles.Button ]
                    , onMouseDown recordStartInspection
                    ]
                    [ text "Hold space to start inspecting" ]

        timer millis =
            div
                [ class [ Styles.Timer ]
                ]
                [ text (elapsedTime (Just millis)) ]

        -- elapsedTime : Maybe Float -> String
        elapsedTime : Maybe Float -> String
        elapsedTime time =
            case time of
                Just time ->
                    let
                        timeInt =
                            round time

                        millisRaw =
                            (timeInt % 1000)

                        -- only display two digits of precision
                        millis =
                            if millisRaw % 10 > 5 then
                                (millisRaw // 10 + 1) % 100
                            else
                                millisRaw // 10

                        secondsRaw =
                            timeInt // 1000

                        seconds =
                            secondsRaw % 60

                        minutes =
                            secondsRaw // 60
                    in
                        (padLeft 2 '0' (toString minutes))
                            ++ ":"
                            ++ (padLeft 2 '0' (toString seconds))
                            ++ "."
                            ++ (padLeft 2 '0' (toString millis))

                _ ->
                    "--:--.00"

        solveInfo model =
            div [ class [ Styles.SolveInfoContainer ] ]
                [ div
                    [ class [ Styles.SolveInfo ]
                    , style [ ( "justifyContent", "space-between" ) ]
                    ]
                    [ div [] [ text "Inspection time:" ]
                    , div
                        [ class [ Styles.SolveInfoTime ] ]
                        [ text (elapsedTime model.inspectionTime) ]
                    ]
                , div
                    [ class [ Styles.SolveInfo ]
                    , style [ ( "justifyContent", "space-between" ) ]
                    ]
                    [ div [] [ text "Solve time:" ]
                    , div
                        [ class [ Styles.SolveInfoTime ] ]
                        [ text (elapsedTime model.solveTime) ]
                    ]
                ]
    in
        div
            [ class [ Styles.Container ]
              {--elm-css doesn't support justifyContent--}
            , style [ ( "justifyContent", "center" ) ]
            ]
            [ timer model.elapsed
            , renderButton
            , solveInfo model
            ]
