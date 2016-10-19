-- TODO: check out https://github.com/cubing/tnoodle/blob/c263d558f2edab8dd325ce256b77e4677d261bc1/scrambles/src/net/gnehzr/tnoodle/scrambles/Puzzle.java
-- https://github.com/jnrbsn/rubiks-cube-scrambler/blob/gh-pages/index.html
-- apparently a random state scramble generator is better
-- https://www.speedsolving.com/forum/threads/wca-scramble-algorithm.12969/
-- https://en.wikipedia.org/wiki/Optimal_solutions_for_Rubik%27s_Cube
-- Kociemba algorithm
{- Current algorithm:

   1. generate initial pair of (F|B|U|D|L|R, 1|2|3)
   2. generate (desired length - 1) more such pairs but taking care not to
           * move opposing face
           * repeat same move
-}


module Scramble exposing (scramble)

import Random exposing (Generator, Seed, int, step)
import Array exposing (Array)
import Move exposing (Move(..))


type alias Moves =
    ( Move, Int )


nextMove : Maybe Move -> Generator Moves
nextMove move =
    let
        opposing =
            case (move) of
                Just F ->
                    Just B

                Just B ->
                    Just F

                Just U ->
                    Just D

                Just D ->
                    Just U

                Just L ->
                    Just R

                Just R ->
                    Just L

                _ ->
                    Nothing

        candidates =
            List.filter (\x -> (Just x) /= move && (Just x) /= opposing) [ F, B, U, D, L, R ]

        array =
            Array.fromList (candidates)

        moveGenerator =
            Random.map
                (\i ->
                    case Array.get i array of
                        Just m ->
                            m

                        -- this case should never happen
                        _ ->
                            F
                )
                (int 0 ((Array.length array) - 1))
    in
        Random.pair moveGenerator (int 1 3)


simplify : Moves -> Maybe Move
simplify moves =
    let
        ( move, times ) =
            moves

        turns =
            times % 4
    in
        case ( move, turns ) of
            ( F, 1 ) ->
                Just F

            ( F, 2 ) ->
                Just F2

            ( F, 3 ) ->
                Just F'

            ( B, 1 ) ->
                Just B

            ( B, 2 ) ->
                Just B2

            ( B, 3 ) ->
                Just B'

            ( U, 1 ) ->
                Just U

            ( U, 2 ) ->
                Just U2

            ( U, 3 ) ->
                Just U'

            ( D, 1 ) ->
                Just D

            ( D, 2 ) ->
                Just D2

            ( D, 3 ) ->
                Just D'

            ( L, 1 ) ->
                Just L

            ( L, 2 ) ->
                Just L2

            ( L, 3 ) ->
                Just L'

            ( R, 1 ) ->
                Just R

            ( R, 2 ) ->
                Just R2

            ( R, 3 ) ->
                Just R'

            _ ->
                Nothing


scramble : Seed -> Int -> List Move
scramble seed targetComplexity =
    let
        -- TODO: this can probably just be a loop?
        go : Generator Moves -> Seed -> List Moves -> List Move
        go generator seed scrambles =
            if List.length scrambles >= targetComplexity then
                List.filterMap simplify scrambles
            else
                let
                    ( next, seed' ) =
                        step generator seed

                    ( move, _ ) =
                        next
                in
                    go (nextMove (Just move)) seed' (next :: scrambles)
    in
        go (nextMove Nothing) seed []
