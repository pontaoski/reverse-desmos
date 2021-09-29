module Shapes exposing (..)


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Line =
    { pos1 : Pos
    , pos2 : Pos
    }


type Shape
    = LineShape Line


toLine : Int -> Int -> Int -> Int -> Line
toLine x1 y1 x2 y2 =
    Line (Pos x1 y1) (Pos x2 y2)


toLineShape : Int -> Int -> Int -> Int -> Shape
toLineShape x1 y1 x2 y2 =
    LineShape (toLine x1 y1 x2 y2)


getYOffset : number -> number -> number -> number
getYOffset m x y =
    -((m * x) + y)


toDesmosRange : Int -> Int -> String
toDesmosRange x1 x2 =
    if x1 < x2 then
        "{" ++ String.fromInt x1 ++ "<=" ++ String.fromInt x2 ++ "}"

    else if x2 < x1 then
        "{" ++ String.fromInt x2 ++ "<=" ++ String.fromInt x1 ++ "}"

    else
        ""


toString : Shape -> String
toString shape =
    case shape of
        LineShape line ->
            let
                x1 =
                    line.pos1.x

                x2 =
                    line.pos2.x

                y1 =
                    line.pos1.y

                y2 =
                    line.pos2.y

                dx =
                    x2 - x1

                dy =
                    y2 - y1
            in
            case ( dx, dy ) of
                ( 0, 0 ) ->
                    "not a line"

                ( 0, _ ) ->
                    "y = " ++ String.fromInt x1

                ( _, 0 ) ->
                    "x = " ++ String.fromInt y1

                ( _, _ ) ->
                    let
                        slope =
                            toFloat dx / toFloat dy

                        yOffset =
                            getYOffset slope (toFloat x1) (toFloat y1)
                    in
                    "y = " ++ String.fromFloat slope ++ "x + " ++ String.fromFloat yOffset ++ toDesmosRange x1 x2
