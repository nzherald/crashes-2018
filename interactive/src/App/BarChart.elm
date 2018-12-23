module App.BarChart exposing (barchart, BarchartOptions)

import Time exposing (Posix)
import Axis
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
import Statistics exposing (extentBy)
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (fontSize, height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..))


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


xScale : Bool -> List ( Posix, Float ) -> BandScale Posix
xScale small model =
    List.map Tuple.first model
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * (padding small) )


xScaleAxis : Bool -> List ( Posix, Float ) -> ContinuousScale Posix
xScaleAxis small model = 
    Scale.time Time.utc ( 0 , w - 2 * (padding small) ) (List.map Tuple.first model 
                    |> extentBy Time.posixToMillis
                    |> Maybe.withDefault (Time.millisToPosix 0, Time.millisToPosix 1))


yScale : Bool -> List ( Posix, Float ) -> ContinuousScale Float
yScale small model =
    Scale.linear ( (h small) - 2 * (padding small), 0 )
        ( 0
        , List.map Tuple.second model
            |> List.maximum
            |> Maybe.withDefault 1
        )


xAxis : Bool -> (Posix -> String) -> Int -> List ( Posix, Float ) -> Svg msg
xAxis small xFormat xTicks model =
    Axis.bottom [ Axis.tickCount xTicks, Axis.tickFormat xFormat ] (xScaleAxis small model)


yAxis : Bool -> List ( Posix, Float ) -> Svg msg
yAxis small model =
    Axis.left [ Axis.tickCount 2 ] (yScale small model)


column : Bool -> (Posix -> String) -> BandScale Posix -> ContinuousScale Float -> ( Posix, Float ) -> Svg msg
column small xFormat scalex scaley ( date, value ) =
    g [ class [ "column" ] ]
        [ rect
            [ x <| Scale.convert scalex date
            , y <| Scale.convert scaley value
            , width <| Scale.bandwidth scalex
            , height <| (h small)- Scale.convert scaley value - 2 * (padding small)
            ]
            []
        , text_
            [ x <| Scale.convert (Scale.toRenderable xFormat scalex) date
            , y <| Scale.convert scaley value - 5
            , textAnchor AnchorMiddle
            ]
            [ text <| String.fromFloat value ]
        ]


type alias BarchartOptions =
    { model : List (Posix, Float)
    , className : String
    , xLabel : String
    , xFormat : Posix -> String
    , xTicks : Int
    , small : Bool
    }

barchart: BarchartOptions -> Svg msg
barchart {model,className, xLabel, xFormat, xTicks, small } =
    let pad = padding small
    in svg
        [ viewBox 0 0 w (h small)
        , class [ "barchart-chart", className ]
        ]
        [ g [ transform [ Translate (pad  - 1) ((h small) - pad) ] ]
            [ xAxis small xFormat xTicks model, text_ [ x (w - 2*pad), y 70, class [ "axis" ] ] [ text xLabel ] ]
        , g [ transform [ Translate (pad - 1) pad ] ]
            [ yAxis small model
            , text_
                [ x 60 , y -30 , class [ "axis" ] ]
                [ text "Crashes" ]
            ]
        , g [ transform [ Translate pad pad ], class [ "series" ] ] <|
            List.map (column small xFormat (xScale small model) (yScale small model)) model
        ]


