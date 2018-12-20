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


h : Float
h =
    350


padding : Float
padding =
    50


xScale : List ( Posix, Float ) -> BandScale Posix
xScale model =
    List.map Tuple.first model
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


xScaleAxis : List ( Posix, Float ) -> ContinuousScale Posix
xScaleAxis model = 
    Scale.time Time.utc ( 0 , w - 2 * padding ) (List.map Tuple.first model 
                    |> extentBy Time.posixToMillis
                    |> Maybe.withDefault (Time.millisToPosix 0, Time.millisToPosix 1))


yScale : List ( Posix, Float ) -> ContinuousScale Float
yScale model =
    Scale.linear ( h - 2 * padding, 0 )
        ( 0
        , List.map Tuple.second model
            |> List.maximum
            |> Maybe.withDefault 1
        )


xAxis : (Posix -> String) -> Int -> List ( Posix, Float ) -> Svg msg
xAxis xFormat xTicks model =
    Axis.bottom [ Axis.tickCount xTicks, Axis.tickFormat xFormat ] (xScaleAxis model)


yAxis : List ( Posix, Float ) -> Svg msg
yAxis model =
    Axis.left [ Axis.tickCount 2 ] (yScale model)


column : (Posix -> String) -> BandScale Posix -> ContinuousScale Float -> ( Posix, Float ) -> Svg msg
column xFormat scalex scaley ( date, value ) =
    g [ class [ "column" ] ]
        [ rect
            [ x <| Scale.convert scalex date
            , y <| Scale.convert scaley value
            , width <| Scale.bandwidth scalex
            , height <| h - Scale.convert scaley value - 2 * padding
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
    }

barchart: BarchartOptions -> Svg msg
barchart {model,className, xLabel, xFormat, xTicks } =
    svg
        [ viewBox 0 0 w h
        , class [ "barchart-chart", className ]
        ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis xFormat xTicks model, text_ [ x (w - 2*padding), y 70, class [ "axis" ] ] [ text xLabel ] ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis model
            , text_
                [ x 60 , y -30 , class [ "axis" ] ]
                [ text "Crashes" ]
            ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column xFormat (xScale model) (yScale model)) model
        ]


