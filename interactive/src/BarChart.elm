module BarChart exposing (barchart)

import Axis
import Scale exposing (BandConfig, BandScale, ContinuousScale, defaultBandConfig)
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
    30


xScale : List ( Int, Float ) -> BandScale Int
xScale model =
    List.map Tuple.first model
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, w - 2 * padding )


yScale : List ( Int, Float ) -> ContinuousScale Float
yScale model =
    Scale.linear ( h - 2 * padding, 0 )
        ( 0
        , List.map Tuple.second model
            |> List.maximum
            |> Maybe.withDefault 1
        )


xAxis : List ( Int, Float ) -> Svg msg
xAxis model =
    Axis.bottom [] (Scale.toRenderable (String.replace "20" "" << String.fromInt) (xScale model))


yAxis : List ( Int, Float ) -> Svg msg
yAxis model =
    Axis.left [ Axis.tickCount 2 ] (yScale model)


column : BandScale Int -> ContinuousScale Float -> ( Int, Float ) -> Svg msg
column scalex scaley ( date, value ) =
    g [ class [ "column" ] ]
        [ rect
            [ x <| Scale.convert scalex date
            , y <| Scale.convert scaley value
            , width <| Scale.bandwidth scalex
            , height <| h - Scale.convert scaley value - 2 * padding
            ]
            []
        , text_
            [ x <| Scale.convert (Scale.toRenderable String.fromInt scalex) date
            , y <| Scale.convert scaley value - 5
            , textAnchor AnchorMiddle
            ]
            [ text <| String.fromFloat value ]
        ]


barchart: String -> List ( Int, Float ) -> Svg msg
barchart cls model =
    svg
        [ viewBox 0 0 w h
        , class [ "barchart-chart", cls ]
        ]
        [ g [ transform [ Translate (padding - 1) (h - padding) ] ]
            [ xAxis model, text_ [ x (w / 2), y 70, class [ "axis" ] ] [ text "Year" ] ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ yAxis model
            , text_
                [ x -(h/2)
                , y -50
                , class [ "axis" ]
                , transform [ Rotate -90 0 0 ]
                ]
                [ text "Crashes" ]
            ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ] <|
            List.map (column (xScale model) (yScale model)) model
        ]


