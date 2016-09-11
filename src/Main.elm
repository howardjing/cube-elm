port module Main exposing (main)

import Html exposing (div, button, text, h3, ul, ol, li)
import Html.Events exposing (onMouseDown, onMouseUp, onClick)
import Time exposing (Time, millisecond)
import Task exposing (Task)
import Keyboard exposing (KeyCode)
import Char exposing (toCode)
import Html.App as App
import MainCss as Styles
import String exposing (padLeft)
import Scramble exposing (scramble, Move)
import Random exposing (initialSeed)


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port requestLatestSolves : () -> Cmd msg


port createSolve : Solve -> Cmd msg


port setLatestSolves : (List Solve -> msg) -> Sub msg


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
    , scramble : List String
    , tags : List String
    }


type alias Model =
    { current : CurrentSolve
    , solves : List Solve
    , scramble : Maybe (List Move)
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.batch
        [ perform requestScramble
        , requestLatestSolves ()
        ]
    )


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
    , scramble = Nothing
    }


type Msg
    = WithTime (Time -> Msg)
    | StartInspection Time
    | StartSolve Time
    | StopSolve Time
    | CreateSolve
    | TickInspection Time
    | TickSolve Time
    | KeyStartInspection KeyCode
    | KeyStartSolve KeyCode
    | KeyStopSolve KeyCode
    | Scramble Float
    | RequestLatestSolves
    | SetLatestSolves (List Solve)


perform : Msg -> Cmd Msg
perform msg =
    Task.perform identity (\_ -> msg) (Task.succeed Nothing)


startSolveTime : CurrentSolve -> Float
startSolveTime solve =
    Maybe.withDefault 0 (Maybe.map2 (+) solve.start solve.inspectionTime)


finalizeSolve : Model -> Solve
finalizeSolve model =
    let
        { current, scramble } =
            model
    in
        { start = Maybe.withDefault 0 current.start
        , inspectionTime = Maybe.withDefault 0 current.inspectionTime
        , solveTime = Maybe.withDefault 0 current.solveTime
        , scramble = []
        , tags = []
        }



-- TODO: can refactor out a lot of the repetitive logic of updating the current solve


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Scramble time ->
            let
                complexity =
                    25
            in
                ( { model | scramble = Just (scramble (initialSeed (round time)) complexity) }, Cmd.none )

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
                    , perform CreateSolve
                    , perform requestScramble
                    ]
                )

        CreateSolve ->
            -- TODO: refactor to only push a solve if all values are present
            let
                { current, scramble } =
                    model

                solve =
                    finalizeSolve model
            in
                ( { model | solves = solve :: model.solves }, createSolve solve )

        RequestLatestSolves ->
            ( model, requestLatestSolves () )

        SetLatestSolves solves ->
            ( { model | solves = solves }, Cmd.none )

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


requestScramble : Msg
requestScramble =
    WithTime (\time -> Scramble time)


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

        base =
            [ setLatestSolves SetLatestSolves
            ]
    in
        if isInspecting current then
            Sub.batch <|
                base
                    ++ [ Time.every (1 * millisecond) TickInspection
                       , Keyboard.ups KeyStartSolve
                       ]
        else if isSolving current then
            Sub.batch <|
                base
                    ++ [ Time.every (1 * millisecond) TickSolve
                       , Keyboard.downs KeyStopSolve
                       ]
        else
            Sub.batch <|
                base
                    ++ [ Keyboard.downs KeyStartInspection
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

                        millis =
                            (timeInt % 1000)

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
                            ++ (padLeft 3 '0' (toString millis))

                _ ->
                    "--:--.---"

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

        active =
            not (isInspecting current) && not (isSolving current)

        renderScramble scramble =
            case scramble of
                Just moves ->
                    div
                        [ class
                            [ Styles.ScrambleContainer
                            , if active then
                                Styles.Active
                              else
                                Styles.Inactive
                            ]
                        ]
                        [ h3 [] [ text "Scramble" ]
                        , ul
                            [ class [ Styles.Scramble ] ]
                            (List.map
                                (\move ->
                                    li [ class [ Styles.ScrambleMove ] ] [ text (toString move) ]
                                )
                                moves
                            )
                        ]

                _ ->
                    div [] []

        renderSolves list =
            let
                solves =
                    List.take 10 list
            in
                if List.length list == 0 then
                    div [] []
                else
                    div
                        [ class
                            [ Styles.SolvesListContainer
                            , if active then
                                Styles.Active
                              else
                                Styles.Inactive
                            ]
                        ]
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
            [ renderScramble model.scramble
            , div
                [ class [ Styles.TimerContainer ] ]
                [ timer current.elapsed
                , renderButton
                , solveInfo current
                ]
            , renderSolves model.solves
            ]
