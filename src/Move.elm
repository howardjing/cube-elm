module Move exposing (Move(..), movesDecoder, moveToJson)

import Json.Decode exposing ((:=))
import Json.Encode


type Move
    = F
    | B
    | U
    | D
    | L
    | R
    | F2
    | B2
    | U2
    | D2
    | L2
    | R2
    | F'
    | B'
    | U'
    | D'
    | L'
    | R'


isNothing : Maybe a -> Bool
isNothing maybe =
    case maybe of
        Nothing ->
            True

        _ ->
            False


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
