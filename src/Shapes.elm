module Shapes exposing (..)

import Json.Decode as D
import Json.Encode as E

type alias Pos =
    { x : Int
    , y : Int
    }


posDelta : Pos -> Int -> Int -> Pos
posDelta pos x y =
    { pos | x = pos.x + x, y = pos.y + y }


type alias Line =
    { pos1 : Pos
    , pos2 : Pos
    }

type alias Circle =
    { centre : Pos
    , radius : Int
    }

type Shape
    = LineShape Line
    | CircleShape Circle


toLine : Int -> Int -> Int -> Int -> Line
toLine x1 y1 x2 y2 =
    Line (Pos x1 y1) (Pos x2 y2)


toLineShape : Int -> Int -> Int -> Int -> Shape
toLineShape x1 y1 x2 y2 =
    LineShape (toLine x1 y1 x2 y2)

toCircle : Int -> Int -> Int -> Circle
toCircle x y r =
    Circle (Pos x y) r

toCircleShape : Int -> Int -> Int -> Shape
toCircleShape x y r =
    CircleShape (toCircle x y r)

getYOffset : number -> number -> number -> number
getYOffset m x y =
    -((m * x) + y)


lbrack : String
lbrack =
    "\\left\\{"


rbrack : String
rbrack =
    "\\right\\}"


le : String
le =
    " \\le "


toDesmosRange : String -> Int -> Int -> String
toDesmosRange var x1 x2 =
    if x1 < x2 then
        lbrack ++ String.fromInt x1 ++ le ++ var ++ le ++ String.fromInt x2 ++ rbrack

    else if x2 < x1 then
        lbrack ++ String.fromInt x2 ++ le ++ var ++ le ++ String.fromInt x1 ++ rbrack

    else
        ""


canvasWidth : number
canvasWidth =
    600


canvasHeight : number
canvasHeight =
    600


toString : Shape -> String
toString shape =
    case shape of
        CircleShape circle ->
            let
                h =
                    circle.centre.x

                k =
                    -circle.centre.y + canvasHeight

                r =
                    circle.radius

                hS =
                    String.fromInt h

                kS =
                    String.fromInt k

                rS =
                    String.fromInt r
            in
                "\\left(x-" ++ hS ++ "\\right)^{2}+\\left(y-" ++ kS ++ "\\right)^{2}=" ++ rS ++ "^{2}"

        LineShape line ->
            let
                x1 =
                    line.pos1.x

                x2 =
                    line.pos2.x

                y1 =
                    line.pos1.y - canvasHeight

                y2 =
                    line.pos2.y - canvasHeight

                dx =
                    x2 - x1

                dy =
                    y2 - y1
            in
            case ( dx, dy ) of
                ( 0, 0 ) ->
                    "not a line"

                ( 0, _ ) ->
                    "x = " ++ String.fromInt x1 ++ toDesmosRange "y" (y1 + canvasHeight) (y2 + canvasHeight)

                ( _, 0 ) ->
                    "y = " ++ String.fromInt y1 ++ toDesmosRange "x" x1 x2

                ( _, _ ) ->
                    let
                        slope =
                            toFloat -dy / toFloat dx

                        yOffset =
                            getYOffset slope (toFloat x1) (toFloat y1)
                    in
                    "y = " ++ String.fromFloat slope ++ "x + " ++ String.fromFloat yOffset ++ toDesmosRange "x" x1 x2

encodePos : Pos -> E.Value
encodePos pos =
    E.object
        [ ("x", E.int pos.x)
        , ("y", E.int pos.y)
        ]

posDecoder : D.Decoder Pos
posDecoder =
    D.map2 Pos
        (D.field "x" D.int)
        (D.field "y" D.int)

encodeLine : Line -> E.Value
encodeLine line =
    E.object
        [ ("kind", E.string "line")
        , ("pos1", encodePos line.pos1)
        , ("pos2", encodePos line.pos2)
        ]

encodeCircle : Circle -> E.Value
encodeCircle circle =
    E.object
        [ ("kind", E.string "circle")
        , ("centre", encodePos circle.centre)
        , ("radius", E.int circle.radius)
        ]

encodeShape : Shape -> E.Value
encodeShape shape =
    case shape of
        LineShape line ->
            encodeLine line

        CircleShape circle ->
            encodeCircle circle

tagDecoder : String -> D.Decoder a -> D.Decoder a
tagDecoder tag decoder =
    D.field "kind" D.string
    |> D.andThen
        (\x ->
            if x == tag then
                decoder
            else
                D.fail "Wrong tag")

lineDecoder : D.Decoder Line
lineDecoder =
    D.map2 Line
        (D.field "pos1" posDecoder)
        (D.field "pos2" posDecoder)
    |> tagDecoder "line"

circleDecoder : D.Decoder Circle
circleDecoder =
    D.map2 Circle
        (D.field "centre" posDecoder)
        (D.field "radius" D.int)
    |> tagDecoder "circle"

shapeDecoder : D.Decoder Shape
shapeDecoder =
    D.oneOf
        [ lineDecoder |> D.map LineShape
        , circleDecoder |> D.map CircleShape
        ]
