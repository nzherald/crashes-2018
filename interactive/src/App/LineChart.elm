module App.LineChart exposing (linechart)

import Axis
import Color
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Statistics exposing (extentBy, extent)
import Shape
import DateFormat
import Time exposing (Posix)
import TypedSvg exposing (g, svg, line)
import TypedSvg.Attributes exposing (class, fill, stroke, transform, viewBox, strokeDasharray)
import TypedSvg.Attributes.InPx exposing (strokeWidth, x1, x2, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..), percent)


type alias Model =
    List ( Posix, Float )


w : Float
w =
    900


h : Float
h =
    350


padding : Float
padding =
    50


xScale : Model -> ContinuousScale Posix
xScale model =
    Scale.time Time.utc ( 0 , w - 2 * padding ) (List.map Tuple.first model 
                    |> extentBy Time.posixToMillis
                    |> Maybe.withDefault (Time.millisToPosix 0, Time.millisToPosix 1))


yScale : Model -> ContinuousScale Float
yScale model =
    Scale.linear ( h - 2 * padding, 0 )
        ( List.map Tuple.second model
            |> extent
            |> Maybe.withDefault (0,1)
            |> \(a,b) -> (a - (b-a)/4, b + (b-a)/4)
        )


xAxis : Model -> Svg msg
xAxis model =
    Axis.bottom [ Axis.tickCount 12, Axis.tickFormat monthFormat ] (xScale model)


yAxis : Model -> Svg msg
yAxis model =
    Axis.left [ Axis.tickCount 5 ] (yScale model)


transformToLineData :
    ContinuousScale Posix
    -> ContinuousScale Float
    -> ( Time.Posix, Float )
    -> Maybe ( Float, Float )
transformToLineData xsl ysl ( x, y ) =
    Just ( Scale.convert xsl x, Scale.convert ysl y )


plotline : Model -> Path
plotline model =
    let
        x =
            xScale model

        y =
            yScale model
    in
    List.map (transformToLineData x y) model
        |> Shape.line Shape.monotoneInXCurve


linechart : String -> String -> Model -> Svg msg
linechart className label model =
    svg [ viewBox 0 0 w h
    , class [ "linechart-chart", className ] 
    ]
        [ g [] <| List.indexedMap (xGridLine <| xScale model) <| Scale.ticks (xScale model) 12
        , g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis model ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element (plotline model) [ stroke (Color.rgb 1 0 0), strokeWidth 5, fill FillNone ]
            ]
        ]


xGridLine : ContinuousScale Posix -> Int -> Posix -> Svg msg
xGridLine scale index tick =
    line
        [ y1 padding
        , y2 (h - padding)
        , x1 ((Scale.convert scale tick) + padding) 
        , x2 ((Scale.convert scale tick) + padding)
        , stroke Color.grey
        , strokeWidth 3
        , strokeDasharray "4"
        ]
        []

monthFormat : Posix -> String
monthFormat =
    DateFormat.format [ DateFormat.monthNameAbbreviated ] Time.utc