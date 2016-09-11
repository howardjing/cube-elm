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
import Scramble exposing (scramble, Move(..))
import Random exposing (initialSeed)
import Json.Decode exposing ((:=))
import Json.Encode


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port requestLatestSolves : () -> Cmd msg


port createSolve : Json.Decode.Value -> Cmd msg


port setLatestSolves : (List Json.Encode.Value -> msg) -> Sub msg


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
    , scramble : List Move
    , tags : List String
    }


type alias Model =
    { current : CurrentSolve
    , solves : List Solve
    , scramble : Maybe (List Move)
    }


movesDecoder : List String -> Json.Decode.Decoder (List Move)
movesDecoder moves =
    let
        decoded =
            List.map
                (\move ->
                    case move of
                        "F" ->
                            Just F

                        "B" ->
                            Just B

                        "U" ->
                            Just U

                        "D" ->
                            Just D

                        "L" ->
                            Just L

                        "R" ->
                            Just R

                        "F2" ->
                            Just F2

                        "B2" ->
                            Just B2

                        "U2" ->
                            Just U2

                        "D2" ->
                            Just D2

                        "L2" ->
                            Just L2

                        "R2" ->
                            Just R2

                        "F'" ->
                            Just F'

                        "B'" ->
                            Just B'

                        "U'" ->
                            Just U'

                        "D'" ->
                            Just D'

                        "L'" ->
                            Just L'

                        "R'" ->
                            Just R'

                        _ ->
                            Nothing
                )
                moves
    in
        if List.any isNothing decoded then
            Json.Decode.succeed []
        else
            Json.Decode.succeed (List.filterMap identity decoded)


moveToJson : Move -> Json.Encode.Value
moveToJson move =
    let
        moveString =
            case move of
                F ->
                    "F"

                B ->
                    "B"

                U ->
                    "U"

                D ->
                    "D"

                L ->
                    "L"

                R ->
                    "R"

                F2 ->
                    "F2"

                B2 ->
                    "B2"

                U2 ->
                    "U2"

                D2 ->
                    "D2"

                L2 ->
                    "L2"

                R2 ->
                    "R2"

                F' ->
                    "F'"

                B' ->
                    "B'"

                U' ->
                    "U'"

                D' ->
                    "D'"

                L' ->
                    "L'"

                R' ->
                    "R'"
    in
        Json.Encode.string moveString


solveDecoder : Json.Decode.Decoder Solve
solveDecoder =
    Json.Decode.object5
        Solve
        ("start" := Json.Decode.float)
        ("inspectionTime" := Json.Decode.float)
        ("solveTime" := Json.Decode.float)
        ("scramble" := Json.Decode.andThen (Json.Decode.list Json.Decode.string) movesDecoder)
        ("tags" := Json.Decode.list Json.Decode.string)


solveToJson : Solve -> Json.Encode.Value
solveToJson solve =
    Json.Encode.object
        [ ( "start", Json.Encode.float solve.start )
        , ( "inspectionTime", Json.Encode.float solve.inspectionTime )
        , ( "solveTime", Json.Encode.float solve.solveTime )
        , ( "scramble", Json.Encode.list <| List.map moveToJson solve.scramble )
        , ( "tags", Json.Encode.list <| List.map Json.Encode.string solve.tags )
        ]


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
        , scramble = Maybe.withDefault [] scramble
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
                ( { model | solves = solve :: model.solves }, createSolve (solveToJson solve) )

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

        decodedSolveJson : List Json.Decode.Value -> Msg
        decodedSolveJson values =
            let
                solves =
                    List.filterMap
                        (\value ->
                            case (Json.Decode.decodeValue solveDecoder value) of
                                Ok solve ->
                                    Just solve

                                _ ->
                                    Nothing
                        )
                        values
            in
                SetLatestSolves solves

        base =
            [ setLatestSolves decodedSolveJson
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


mean : List Float -> Maybe Float
mean list =
    if List.length list == 0 then
        Nothing
    else
        Just <| (List.sum list) / (toFloat (List.length list))


avg : List Float -> Maybe Float
avg list =
    if List.length list <= 2 then
        Nothing
    else
        let
            len =
                List.length list

            pruned =
                list
                    |> List.sort
                    |> List.tail
                    |> Maybe.withDefault []
                    |> List.take (len - 2)
        in
            mean pruned


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
                [ Just millis
                    |> elapsedTime
                    |> text
                ]

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
                        [ current.inspectionTime
                            |> elapsedTime
                            |> text
                        ]
                    ]
                , div
                    [ class [ Styles.SolveInfo ] ]
                    [ div [] [ text "Solve time:" ]
                    , div
                        [ class [ Styles.SolveInfoTime ] ]
                        [ current.solveTime
                            |> elapsedTime
                            |> text
                        ]
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
                                    li
                                        [ class [ Styles.ScrambleMove ] ]
                                        [ move
                                            |> toString
                                            |> text
                                        ]
                                )
                                moves
                            )
                        ]

                _ ->
                    div [] []

        renderSolves : List Solve -> Html.Html Msg
        renderSolves list =
            let
                solves =
                    list
                        |> List.take 12
                        |> List.map .solveTime
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
                                    li []
                                        [ Just solve
                                            |> elapsedTime
                                            |> text
                                        ]
                                )
                                solves
                            )
                        , div
                            [ class [ Styles.SolvesListStats ] ]
                            [ div
                                [ class [] ]
                                [ solves
                                    |> List.minimum
                                    |> elapsedTime
                                    |> (++) "min: "
                                    |> text
                                ]
                            , div
                                [ class [] ]
                                [ solves
                                    |> avg
                                    |> elapsedTime
                                    |> (++) "avg: "
                                    |> text
                                ]
                            ]
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
