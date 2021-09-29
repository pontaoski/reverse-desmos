module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Draggable
import Draggable.Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Html exposing (Html)
import Shapes
import Svg
import Svg.Attributes as Attributes



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


shadowColor : Color
shadowColor =
    rgba255 36 36 54 0.2



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


type DragTarget
    = LinePos1
    | LinePos2


type alias ShapeId =
    String


type alias Id =
    { name : ShapeId
    , target : DragTarget
    }


plainLine : Shapes.Shape
plainLine =
    Shapes.toLineShape 0 0 200 200


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialModel =
            { shapes = Dict.fromList [ ( "hi", plainLine ) ]
            , drag = Draggable.init
            , currentlyDragging = Nothing
            , currentGeneratorShapeId = "a"
            }
    in
    ( initialModel, Cmd.none )


type Msg
    = DragMsg (Draggable.Msg Id)
    | OnDragBy Draggable.Delta
    | StartDrag Id
    | StopDrag
    | NewLine


dragConfig : Draggable.Config Id Msg
dragConfig =
    Draggable.customConfig
        [ Draggable.Events.onDragBy OnDragBy
        , Draggable.Events.onDragStart StartDrag
        , Draggable.Events.onDragEnd StopDrag
        ]


type alias Model =
    { shapes : Dict ShapeId Shapes.Shape
    , drag : Draggable.State Id
    , currentlyDragging : Maybe Id
    , currentGeneratorShapeId : ShapeId
    }



-- UPDATE


updateDict : comparable -> (value -> value) -> Dict comparable value -> Dict comparable value
updateDict key mapper dict =
    Dict.update key (Maybe.map mapper) dict


updateShape : Int -> Int -> DragTarget -> Shapes.Shape -> Shapes.Shape
updateShape x y target shape =
    case target of
        LinePos1 ->
            case shape of
                Shapes.LineShape { pos1, pos2 } ->
                    Shapes.LineShape (Shapes.Line (Shapes.posDelta pos1 x y) pos2)

        LinePos2 ->
            case shape of
                Shapes.LineShape { pos1, pos2 } ->
                    Shapes.LineShape (Shapes.Line pos1 (Shapes.posDelta pos2 x y))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DragMsg dragMsg ->
            Draggable.update dragConfig dragMsg model

        OnDragBy ( x, y ) ->
            case model.currentlyDragging of
                Just it ->
                    ( { model | shapes = model.shapes |> updateDict it.name (\shape -> updateShape (round x) (round y) it.target shape) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        StartDrag id ->
            ( { model | currentlyDragging = Just id }, Cmd.none )

        StopDrag ->
            ( { model | currentlyDragging = Nothing }, Cmd.none )

        NewLine ->
            ( { model
                | currentGeneratorShapeId = model.currentGeneratorShapeId ++ "a"
                , shapes = Dict.insert model.currentGeneratorShapeId plainLine model.shapes
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions { drag } =
    Draggable.subscriptions DragMsg drag



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
        (List.map Shapes.toString (Dict.values model.shapes) |> List.map label |> List.map (\elm -> paragraph [] [ elm ]))


renderCircle : Int -> Int -> Int -> Html.Attribute Msg -> Html Msg
renderCircle x y r a =
    Svg.circle
        [ Attributes.cx (String.fromInt x)
        , Attributes.cy (String.fromInt y)
        , Attributes.r (String.fromInt r)
        , Attributes.style "fill: rgba(0, 0, 0, 0.5);"
        , a
        ]
        []


renderLine : ( ShapeId, Shapes.Line ) -> Html Msg
renderLine ( id, line ) =
    Svg.g
        []
        [ Svg.line
            [ Attributes.x1 (String.fromInt line.pos1.x)
            , Attributes.x2 (String.fromInt line.pos2.x)
            , Attributes.y1 (String.fromInt line.pos1.y)
            , Attributes.y2 (String.fromInt line.pos2.y)
            , Attributes.style "stroke: rgb(0,0,0); stroke-width: 1.5;"
            ]
            []
        , renderCircle line.pos1.x line.pos1.y 10 (Draggable.mouseTrigger (Id id LinePos1) DragMsg)
        , renderCircle line.pos2.x line.pos2.y 10 (Draggable.mouseTrigger (Id id LinePos2) DragMsg)

        --      , Svg.text_ [ Attributes.x (String.fromInt line.pos1.x), Attributes.y (String.fromInt (line.pos1.y - 20)) ] [ Svg.text ((String.fromInt line.pos1.x) ++ ", " ++ (String.fromInt line.pos1.y)) ]
        --      , Svg.text_ [ Attributes.x (String.fromInt line.pos2.x), Attributes.y (String.fromInt (line.pos2.y - 20)) ] [ Svg.text ((String.fromInt line.pos2.x) ++ ", " ++ (String.fromInt line.pos2.y)) ]
        ]


getLine : ( ShapeId, Shapes.Shape ) -> Maybe ( ShapeId, Shapes.Line )
getLine ( id, shape ) =
    case shape of
        Shapes.LineShape line ->
            Just ( id, line )


lines : Model -> List ( ShapeId, Shapes.Line )
lines { shapes } =
    List.filterMap getLine (Dict.toList shapes)


renderGraph : Model -> Html Msg
renderGraph model =
    Svg.svg
        [ Attributes.width (String.fromInt Shapes.canvasWidth)
        , Attributes.height (String.fromInt Shapes.canvasHeight)
        ]
        (lines model |> List.map renderLine)


graphContainer : Model -> Element Msg
graphContainer model =
    el
        [ Border.shadow
            { offset = ( 0.0, 4.0 )
            , size = 5.0
            , blur = 5.0
            , color = shadowColor
            }
        ]
        (html (renderGraph model))


separator : Color
separator =
    rgb255 206 210 213


button : Color
button =
    rgb255 247 247 247


focus : Color
focus =
    rgb255 61 174 233


content : Model -> Element Msg
content model =
    column
        [ width fill
        , height fill
        , padding 16
        , scrollbarY
        , Background.color contentColor
        ]
        [ column
            [ centerX ]
            [ graphContainer model
            , row [ centerX, padding 16 ]
                [ Input.button
                    [ Background.color button
                    , Border.solid
                    , Border.color separator
                    , Border.width 1
                    , Border.rounded 3
                    , padding 5
                    , mouseDown
                        [ Background.color focus
                        ]
                    ]
                    { onPress = Just NewLine
                    , label = text "New Line"
                    }
                ]
            ]
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
