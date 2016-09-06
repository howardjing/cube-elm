module Main exposing (main)

import Html exposing (div, button, text, h3, ol, li)
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


type alias CurrentSolve =
    { start : Maybe Time
    , inspectionTime : Maybe Float
    , solveTime : Maybe Float
    , elapsed : Float
    }


type alias Solve =
    { start : Time
    , inspectionTime : Float
    , solveTime : Float
    }


type alias Model =
    { current : CurrentSolve
    , solves : List Solve
    }


initialSolve : CurrentSolve
initialSolve =
    { start = Nothing
    , inspectionTime = Nothing
    , solveTime = Nothing
    , elapsed = 0
    }


initialModel : Model
initialModel =
    { current = initialSolve
    , solves = []
    }


type Msg
    = WithTime (Time -> Msg)
    | StartInspection Time
    | StartSolve Time
    | StopSolve Time
    | AddSolve
    | TickInspection Time
    | TickSolve Time
    | KeyStartInspection KeyCode
    | KeyStartSolve KeyCode
    | KeyStopSolve KeyCode


perform : Msg -> Cmd Msg
perform msg =
    Task.perform identity (\_ -> msg) (Task.succeed Nothing)


startSolveTime : CurrentSolve -> Float
startSolveTime solve =
    Maybe.withDefault 0 (Maybe.map2 (+) solve.start solve.inspectionTime)



-- TODO: can refactor out a lot of the repetitive logic of updating the current solve


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartInspection time ->
            let
                newSolve =
                    { initialSolve | start = Just time }
            in
                ( { model | current = newSolve }, Cmd.none )

        StartSolve time ->
            let
                { current } =
                    model

                updatedSolve =
                    { current | inspectionTime = Maybe.map (\start -> time - start) current.start }
            in
                ( { model | current = updatedSolve }
                , perform (TickInspection time)
                )

        StopSolve time ->
            let
                { current } =
                    model

                updatedSolve =
                    { current | solveTime = Just (time - (startSolveTime current)) }
            in
                ( { model | current = updatedSolve }
                , Cmd.batch
                    [ perform (TickSolve time)
                    , perform AddSolve
                    ]
                )

        AddSolve ->
            -- TODO: refactor to only push a solve if all values are present
            let
                { current } =
                    model

                solve =
                    { start = Maybe.withDefault 0 current.start
                    , inspectionTime = Maybe.withDefault 0 current.inspectionTime
                    , solveTime = Maybe.withDefault 0 current.solveTime
                    }
            in
                ( { model | solves = solve :: model.solves }, Cmd.none )

        TickInspection time ->
            let
                { current } =
                    model

                updatedSolve =
                    { current | elapsed = time - (Maybe.withDefault 0 current.start) }
            in
                ( { model | current = updatedSolve }, Cmd.none )

        TickSolve time ->
            let
                { current } =
                    model

                updatedSolve =
                    { current | elapsed = time - (startSolveTime current) }
            in
                ( { model | current = updatedSolve }, Cmd.none )

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


isInspecting : CurrentSolve -> Bool
isInspecting model =
    (not (isNothing model.start)) && (isNothing model.inspectionTime)


isSolving : CurrentSolve -> Bool
isSolving model =
    (not (isNothing model.start))
        && (not (isNothing model.inspectionTime))
        && (isNothing model.solveTime)


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        { current } =
            model
    in
        if isInspecting current then
            Sub.batch
                [ Time.every (1 * millisecond) TickInspection
                , Keyboard.ups KeyStartSolve
                ]
        else if isSolving current then
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
        { current } =
            model

        renderButton =
            if isInspecting current then
                button
                    [ class [ Styles.Button ]
                    , onMouseUp recordStartSolve
                    ]
                    [ text "Release space when finished inspecting" ]
            else if isSolving current then
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
                    "--:--.--"

        solveInfo : CurrentSolve -> Html.Html Msg
        solveInfo current =
            div [ class [ Styles.SolveInfoContainer ] ]
                [ div
                    [ class [ Styles.SolveInfo ] ]
                    [ div [] [ text "Inspection time:" ]
                    , div
                        [ class [ Styles.SolveInfoTime ] ]
                        [ text (elapsedTime current.inspectionTime) ]
                    ]
                , div
                    [ class [ Styles.SolveInfo ] ]
                    [ div [] [ text "Solve time:" ]
                    , div
                        [ class [ Styles.SolveInfoTime ] ]
                        [ text (elapsedTime current.solveTime) ]
                    ]
                ]

        renderSolves list =
            let
                solves =
                    List.take 10 list
            in
                if List.length list == 0 then
                    div [] []
                else
                    div [ class [ Styles.SolvesListContainer ] ]
                        [ h3 []
                            [ text "Previous Solves" ]
                        , ol
                            [ class [ Styles.SolvesList ] ]
                            (List.map
                                (\solve ->
                                    li [] [ text (elapsedTime (Just solve.solveTime)) ]
                                )
                                solves
                            )
                        ]
    in
        div
            [ class [ Styles.Container ] ]
            [ div
                [ class [ Styles.TimerContainer ] ]
                [ timer current.elapsed
                , renderButton
                , solveInfo current
                ]
            , (renderSolves model.solves)
            ]
