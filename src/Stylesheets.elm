port module Stylesheets exposing (main)

import Css.File exposing (CssFileStructure, toFileStructure, compile)
import Html exposing (div)
import Html.App as App
import MainCss


port files : CssFileStructure -> Cmd msg


cssFiles : CssFileStructure
cssFiles =
    toFileStructure [ ( "main.css", compile [ MainCss.css ] ) ]


main : Program Never
main =
    App.program
        { init = ( (), files cssFiles )
        , view = \_ -> (div [] [])
        , update = \_ _ -> ( (), Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
