module Main exposing (main)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Shapes
import Svg
import Svg.Attributes as Attributes
import Task
import Time



-- COLORS


sideColor : Color
sideColor =
    rgb255 239 240 241


contentColor : Color
contentColor =
    rgb255 255 255 255


headerColor : Color
headerColor =
    rgb255 222 224 226



-- MAIN


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { shapes : List Shapes.Shape
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model [ Shapes.toLineShape 0 0 200 200 ]
    , Cmd.none
    )


type Msg
    = Nichts



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Desmos But Not Pain"
    , body = [ layout [] (body model) ]
    }


label : String -> Element Msg
label txt =
    el [] (text txt)


sidebar : Model -> Element Msg
sidebar model =
    column
        [ Background.color sideColor
        , width (px 200)
        , height fill
        ]
        (List.map Shapes.toString model.shapes |> List.map label)


renderLine : Shapes.Line -> Html Msg
renderLine line =
    Svg.line
        [ Attributes.x1 (String.fromInt line.pos1.x)
        , Attributes.x2 (String.fromInt line.pos2.x)
        , Attributes.y1 (String.fromInt line.pos1.y)
        , Attributes.y2 (String.fromInt line.pos2.y)
        , Attributes.style "stroke: rgb(0,0,0); stroke-width: 1.5;"
        ]
        []


getLine : Shapes.Shape -> Maybe Shapes.Line
getLine shape =
    case shape of
        Shapes.LineShape line ->
            Just line


lines : Model -> List Shapes.Line
lines model =
    List.filterMap getLine model.shapes


renderGraph : Model -> Html Msg
renderGraph model =
    Svg.svg
        [ Attributes.width "500"
        , Attributes.height "500"
        ]
        (lines model |> List.map renderLine)


content : Model -> Element Msg
content model =
    column
        [ width fill
        , height fill
        , Background.color contentColor
        ]
        [ html (renderGraph model)
        ]


toolbar : a -> Element Msg
toolbar _ =
    column
        [ height (px 48)
        , width fill
        , Background.color headerColor
        ]
        [ el [ centerX, centerY ] (label "desmos but it gives you equations instead of you giving it equations")
        ]


body : Model -> Element Msg
body model =
    column [ width fill, height fill ]
        [ toolbar model
        , row
            [ width fill
            , height fill
            ]
            [ sidebar model
            , content model
            ]
        ]
