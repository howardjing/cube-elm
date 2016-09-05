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
        , fontSize
        , px
        , flex
        , int
        , marginRight
        , marginBottom
        , margin
        , padding
        , minWidth
        , width
        , fontFamilies
        )
import Css.Namespace
import Html.CssHelpers exposing (withNamespace)


namespace =
    withNamespace "main"


type CssClasses
    = Button
    | Timer
    | Container
    | SolveInfoContainer
    | SolveInfo
    | SolveInfoTime


css =
    (stylesheet << Css.Namespace.namespace namespace.name)
        [ ((.) Button)
            [ width (px 250)
            , backgroundColor (hex "ccffaa")
            , padding (px 10)
            ]
        , ((.) Timer)
            [ fontFamilies [ "monospace" ]
            , fontSize (px 72)
            , marginBottom (px 10)
            ]
        , ((.) Container)
            [ displayFlex
            , flexDirection column
            , alignItems center
            , flex (int 1)
            ]
        , ((.) SolveInfoContainer)
            [ displayFlex
            , flexDirection column
            , minWidth (px 180)
            , margin (px 20)
            ]
        , ((.) SolveInfo)
            [ displayFlex
            ]
        , ((.) SolveInfoTime)
            [ fontFamilies [ "monospace" ]
            ]
        ]
