module Main exposing (..)

import Html exposing (div, text, span)
import Color exposing (..)
import Math.Vector3 exposing (..)
import Math.Matrix4 exposing (..)
import WebGL exposing (..)
import Html.App as Html
import Html.Attributes exposing (width, height)
import Mouse
import Html.Events exposing (onMouseOver, onMouseLeave)


main : Program Never
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = SetMouse Int Int
    | SetHovering Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetMouse x y ->
            ( { model | x = x, y = y }, Cmd.none )

        SetHovering hovering ->
            ( { model | hovering = hovering }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { hovering } =
    if hovering then
        Sub.batch
            [ Mouse.moves (\{ x, y } -> SetMouse x y)
            ]
    else
        Sub.batch []


type alias Model =
    { x : Int
    , y : Int
    , hovering : Bool
    }


initialModel : Model
initialModel =
    { x = 0
    , y = 0
    , hovering = False
    }


coords : Model -> Html.Html Msg
coords { x, y } =
    div []
        [ text <| "(" ++ (toString x) ++ "," ++ (toString y) ++ ")" ]


view : Model -> Html.Html Msg
view model =
    div []
        [ span
            [ (onMouseOver (SetHovering True))
            , (onMouseLeave (SetHovering False))
            ]
            [ WebGL.toHtml [ width 400, height 400 ] (scene 0) ]
        , span
            [ (onMouseOver (SetHovering True))
            , (onMouseLeave (SetHovering False))
            ]
            [ WebGL.toHtml [ width 400, height 400 ] (scene 0) ]
        , (coords model)
        ]



-- MESHES - create a cube in which each vertex has a position and color


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


triangle : Drawable Vertex
triangle =
    Triangle
        [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
          , Vertex (vec3 1 1 0) (vec3 0 1 0)
          , Vertex (vec3 1 -1 0) (vec3 0 0 1)
          )
        ]


cube : Vec3 -> Drawable Vertex
cube translation =
    let
        rft =
            add (vec3 0.5 0.5 0.5) translation

        -- right, front, top
        lft =
            add (vec3 -0.5 0.5 0.5) translation

        -- left,  front, top
        lbt =
            add (vec3 -0.5 -0.5 0.5) translation

        rbt =
            add (vec3 0.5 -0.5 0.5) translation

        rbb =
            add (vec3 0.5 -0.5 -0.5) translation

        rfb =
            add (vec3 0.5 0.5 -0.5) translation

        lfb =
            add (vec3 -0.5 0.5 -0.5) translation

        lbb =
            add (vec3 -0.5 -0.5 -0.5) translation
    in
        Triangle
            << List.concat
        <|
            [ face green rft rfb rbb rbt
              -- right
            , face blue rft rfb lfb lft
              -- front
            , face yellow rft lft lbt rbt
              -- top
            , face red rfb lfb lbb rbb
              -- bottom
            , face purple lft lfb lbb lbt
              -- left
            , face orange rbt rbb lbb lbt
              -- back
            ]


face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
face rawColor a b c d =
    let
        color =
            let
                c =
                    toRgb rawColor
            in
                vec3
                    (toFloat c.red / 255)
                    (toFloat c.green / 255)
                    (toFloat c.blue / 255)

        vertex position =
            Vertex color position
    in
        [ ( vertex a, vertex b, vertex c )
        , ( vertex c, vertex d, vertex a )
        ]



-- VIEW


scene : Float -> List Renderable
scene angle =
    [ render cubeShader fragmentShader (cube (vec3 0 0 0)) (uniforms angle)
    , render cubeShader fragmentShader (cube (vec3 1.01 0 0)) (uniforms angle)
    , render cubeShader fragmentShader (cube (vec3 2.02 0 0)) (uniforms angle)
    , render cubeShader fragmentShader (cube (vec3 0 1.01 0)) (uniforms angle)
    , render cubeShader fragmentShader (cube (vec3 0 2.02 0)) (uniforms angle)
    , render cubeShader fragmentShader (cube (vec3 1.01 1.01 0)) (uniforms angle)
    , render cubeShader fragmentShader (cube (vec3 1.01 2.02 0)) (uniforms angle)
    , render cubeShader fragmentShader (cube (vec3 2.02 1.01 0)) (uniforms angle)
    , render cubeShader fragmentShader (cube (vec3 2.02 2.02 0)) (uniforms angle)
    ]


uniforms : Float -> { rotation : Mat4, perspective : Mat4, camera : Mat4, shade : Float }
uniforms t =
    { rotation = mul (makeRotate (3 * t) (vec3 0 1 0)) (makeRotate (2 * t) (vec3 1 0 0))
    , perspective = makePerspective 45 1 0.01 100
    , camera = makeLookAt (vec3 0 0 10) (vec3 0 0 0) (vec3 0 1 0)
    , shade = 0.8
    }



-- SHADERS


cubeShader : Shader { attr | position : Vec3, color : Vec3 } { unif | rotation : Mat4, perspective : Mat4, camera : Mat4 } { vcolor : Vec3 }
cubeShader =
    [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
varying vec3 vcolor;
void main () {
    gl_Position = perspective * camera * vec4(position, 1.0);
    vcolor = color;
}

|]


vertexShader : Shader { attr | position : Vec3, color : Vec3 } { unif | rotation : Mat4, perspective : Mat4, camera : Mat4 } { vcolor : Vec3 }
vertexShader =
    [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 rotation;
varying vec3 vcolor;
void main () {
    gl_Position = perspective * camera * (vec4(position, 1.0));
    vcolor = color;
}

|]


fragmentShader : Shader {} { u | shade : Float } { vcolor : Vec3 }
fragmentShader =
    [glsl|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]
