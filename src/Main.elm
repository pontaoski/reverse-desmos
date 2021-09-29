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
import Html.Events.Extra.Mouse as Mouse
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
    | Canvas


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
            { shapes = Dict.fromList []
            , drag = Draggable.init
            , currentlyDragging = Nothing
            , currentGeneratorShapeId = "a"
            , newLinePos1 = (0, 0)
            , showCircles = True
            }
    in
    ( initialModel, Cmd.none )


type Msg
    = DragMsg (Draggable.Msg Id)
    | OnDragBy Draggable.Delta
    | StartDrag Id
    | StopDrag
    | NewLine
    | MouseDown Int Int
    | ToggleCircles


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
    , newLinePos1 : ( Int, Int )
    , showCircles : Bool
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

        Canvas ->
            shape


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
            case id.target of
                Canvas ->
                    let
                        newID =
                            model.currentGeneratorShapeId ++ "a"

                        x1 =
                            Tuple.first model.newLinePos1

                        y1 =
                            Tuple.second model.newLinePos1
                    in
                    ( { model
                        | currentGeneratorShapeId = newID
                        , shapes = Dict.insert newID (Shapes.toLineShape x1 y1 x1 y1) model.shapes
                        , currentlyDragging = Just (Id newID LinePos2)
                      }
                    , Cmd.none
                    )

                _ ->
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

        MouseDown x y ->
            ( { model | newLinePos1 = (x, y) }, Cmd.none )

        ToggleCircles ->
            ( { model | showCircles = not model.showCircles}, Cmd.none )



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


renderLine : Bool -> ( ShapeId, Shapes.Line ) -> Html Msg
renderLine showCircles ( id, line ) =
    Svg.g
        []
        ([ Svg.line
            [ Attributes.x1 (String.fromInt line.pos1.x)
            , Attributes.x2 (String.fromInt line.pos2.x)
            , Attributes.y1 (String.fromInt line.pos1.y)
            , Attributes.y2 (String.fromInt line.pos2.y)
            , Attributes.style "stroke: rgb(0,0,0); stroke-width: 1.5;"
            ]
            []
        ] ++ (if showCircles then
            [ renderCircle line.pos1.x line.pos1.y 10 (Draggable.mouseTrigger (Id id LinePos1) DragMsg)
            , renderCircle line.pos2.x line.pos2.y 10 (Draggable.mouseTrigger (Id id LinePos2) DragMsg)
            ] else []))


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
        , Draggable.mouseTrigger (Id "" Canvas) DragMsg
        , Mouse.onMove (\ev -> MouseDown (round (Tuple.first ev.offsetPos)) (round (Tuple.second ev.offsetPos)))
        ]
        (lines model |> List.map (renderLine model.showCircles))


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


buttonColor : Color
buttonColor =
    rgb255 247 247 247


focus : Color
focus =
    rgb255 61 174 233

button { onPress, textLabel } =
    Input.button
        [ Background.color buttonColor
        , Border.solid
        , Border.color separator
        , Border.width 1
        , Border.rounded 3
        , padding 5
        , mouseDown
            [ Background.color focus
            ]
        ]
        { onPress = onPress
        , label = text textLabel
        }

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
            , row [ centerX, padding 16, spacing 6 ]
                [ button { onPress = Just NewLine, textLabel = "New Line" }
                , button { onPress = Just ToggleCircles, textLabel = if model.showCircles then "Hide Circles" else "Show Circles" }
                ]
            ]
        ]


toolbar : Model -> Element Msg
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
