module MainCss exposing (css, namespace, CssClasses(..))

import Css exposing (Stylesheet, stylesheet, (.), backgroundColor, hex)
import Css.Namespace
import Html.CssHelpers exposing (withNamespace)


-- name : Html.CssHelpers.Namespace


namespace =
    withNamespace "main"


type CssClasses
    = Button
    | Timer


css =
    (stylesheet << Css.Namespace.namespace namespace.name)
        [ ((.) Button)
            [ backgroundColor (hex "ccffaa") ]
        , ((.) Timer)
            [ backgroundColor (hex "eeeeee") ]
        ]
