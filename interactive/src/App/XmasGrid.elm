module App.XmasGrid exposing (xmasGrid)

import App.DataTypes exposing (..)
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, textAnchor, transform, viewBox, fill, stroke)
import TypedSvg.Attributes.InPx exposing (fontSize, height, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Transform(..), Fill(..))
import Color

margin = 50

xmasGrid : Float -> Float -> List Xmas -> Svg msg
xmasGrid w h xmases =
    let
        dayHeight =
            h / (toFloat <| List.length xmases)

        dayWidth =
            (w - (2*margin))/13

        hourHeight =
            (dayHeight / 24) * 0.9

        xmas i x =
            g [ transform [ Translate margin (toFloat i * dayHeight) ] ]
                (List.indexedMap (days i) x.days)

        days i j d =
            g [ transform [ Translate (toFloat j * dayWidth) 0 ] ]
                (List.indexedMap (hours i i) d.hours)

        hours i j k { fatal, serious } =
            rect
                [ width dayWidth
                , height hourHeight
                , x 0
                , y (toFloat k * hourHeight)
                , fill FillNone
                , stroke Color.grey
                ]
                []
    in
    svg [ viewBox 0 0 w h ] (List.indexedMap xmas xmases)
