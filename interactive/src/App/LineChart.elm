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


h small = if small then hs else hb

hb : Float
hb =
    350


hs : Float
hs = 180


padding small = if small then paddingSmall else paddingBig

paddingSmall : Float
paddingSmall =
    28

paddingBig : Float
paddingBig =
    50


xScale : Bool -> Model -> ContinuousScale Posix
xScale small model =
    Scale.time Time.utc ( 0 , w - 2 * (padding small) ) (List.map Tuple.first model 
                    |> extentBy Time.posixToMillis
                    |> Maybe.withDefault (Time.millisToPosix 0, Time.millisToPosix 1))


yScale : Bool -> Model -> ContinuousScale Float
yScale small model =
    Scale.linear ( (h small) - 2 * (padding small), 0 )
        ( List.map Tuple.second model
            |> extent
            |> Maybe.withDefault (0,1)
            |> \(a,b) -> (a - (b-a)/4, b + (b-a)/4)
        )


xAxis : Bool -> Model -> Svg msg
xAxis small model =
    Axis.bottom [ Axis.tickCount 12, Axis.tickFormat monthFormat ] (xScale small model)


yAxis : Bool -> Model -> Svg msg
yAxis small model =
    Axis.left [ Axis.tickCount 5 ] (yScale small model)


transformToLineData :
    ContinuousScale Posix
    -> ContinuousScale Float
    -> ( Time.Posix, Float )
    -> Maybe ( Float, Float )
transformToLineData xsl ysl ( x, y ) =
    Just ( Scale.convert xsl x, Scale.convert ysl y )


plotline : Bool -> Model -> Path
plotline small model =
    let
        x =
            xScale small model

        y =
            yScale small model
    in
    List.map (transformToLineData x y) model
        |> Shape.line Shape.monotoneInXCurve


linechart : Bool -> String -> String -> Model -> Svg msg
linechart small className label model =
    let pad = padding small
    in svg [ viewBox 0 0 w (h small)
    , class [ "linechart-chart", className ] 
    ]
        [ g [] <| List.indexedMap (xGridLine small <| xScale small model) <| Scale.ticks (xScale small model) 12
        , g [ transform [ Translate (pad - 1) ((h small) - pad) ] ]
            [ xAxis small model ]
        , g [ transform [ Translate (pad - 1) pad ] ]
            [ yAxis small model ]
        , g [ transform [ Translate pad pad ], class [ "series" ] ]
            [ Path.element (plotline small model) [ stroke (Color.rgb 1 0 0), strokeWidth 5, fill FillNone ]
            ]
        ]


xGridLine : Bool -> ContinuousScale Posix -> Int -> Posix -> Svg msg
xGridLine small scale index tick =
    let pad = padding small
    in line
        [ y1 pad
        , y2 ((h small) - pad)
        , x1 ((Scale.convert scale tick) + pad) 
        , x2 ((Scale.convert scale tick) + pad)
        , stroke Color.grey
        , strokeWidth 3
        , strokeDasharray "4"
        ]
        []

monthFormat : Posix -> String
monthFormat =
    DateFormat.format [ DateFormat.monthNameAbbreviated ] Time.utc