module MainCss exposing (css, namespace, CssClasses(..))

import Css
    exposing
        ( Stylesheet
        , stylesheet
        , (.)
        , backgroundColor
        , hex
        , displayFlex
        , alignItems
        , flexDirection
        , center
        , column
        , row
        , fontSize
        , px
        , flex
        , int
        , marginRight
        , marginBottom
        , margin
        , padding
        , padding2
        , padding4
        , minWidth
        , width
        , fontFamilies
        , solid
        , border3
        , borderRadius
        , hex
        , property
        , cursor
        , pointer
        , outline
        , zero
        , position
        , fixed
        , top
        , right
        , none
        )
import Css.Namespace
import Html.CssHelpers exposing (withNamespace)


namespace =
    withNamespace "main"


type CssClasses
    = Button
    | Container
    | TimerContainer
    | Timer
    | SolveInfoContainer
    | SolveInfo
    | SolveInfoTime
      -- solves list
    | SolvesListContainer
    | SolvesList


css =
    (stylesheet << Css.Namespace.namespace namespace.name)
        [ ((.) Button)
            [ width (px 350)
            , border3 (px 1) solid (hex "000")
            , borderRadius (px 5)
            , backgroundColor (hex "fff")
            , padding2 (px 15) (px 0)
            , fontSize (px 16)
            , cursor pointer
            , outline zero
            ]
        , ((.) Timer)
            [ fontFamilies [ "Roboto Mono", "monospace" ]
            , fontSize (px 72)
            , marginBottom (px 10)
            ]
        , ((.) Container)
            [ displayFlex
            , flexDirection row
            , flex (int 1)
            ]
        , ((.) TimerContainer)
            [ displayFlex
            , flexDirection column
            , alignItems center
            , flex (int 1)
            , property "justify-content" "center"
            ]
        , ((.) SolveInfoContainer)
            [ displayFlex
            , flexDirection column
            , minWidth (px 200)
            , margin (px 20)
            ]
        , ((.) SolveInfo)
            [ displayFlex
            , property "justify-content" "space-between"
            ]
        , ((.) SolveInfoTime)
            [ fontFamilies [ "Roboto Mono", "monospace" ]
            ]
          -- Solves List
        , ((.) SolvesListContainer)
            [ position fixed
            , top zero
            , right zero
            , padding (px 20)
            ]
        , ((.) SolvesList)
            [ margin zero
            , padding4 (px 0) (px 0) (px 0) (px 20)
            ]
        ]
