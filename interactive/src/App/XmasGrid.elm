module App.XmasGrid exposing (xmasGrid)

import App.DataTypes exposing (..)
import Color exposing (rgb255)
import TypedSvg exposing (circle, g, rect, svg, text_)
import TypedSvg.Attributes exposing (class, fill, fontFamily, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, height, r, width, strokeWidth, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..))


margin =
    50


xmasGrid : Float -> Float -> List Xmas -> Svg msg
xmasGrid w h xmases =
    let
        dayHeight =
            h / (toFloat <| List.length xmases)

        dayWidth =
            (w - (2 * margin)) / 13

        hourHeight =
            (dayHeight / 24) * 0.9

        xmas i x =
            g [ transform [ Translate margin (toFloat i * dayHeight) ] ]
                (List.indexedMap (days i) x.days)

        days i j d =
            g [ transform [ Translate (toFloat j * dayWidth) 0 ] ]
                (List.map hours d.hours)

        dot base col i =
            circle
                [ cx (toFloat (base + i) * (hourHeight * 1.1) + hourHeight)
                , cy (hourHeight * 0.5)
                , r (hourHeight * 0.45)
                , fill <| Fill col
                ]
                []

        dotList base count color =
            List.range 0 count
                |> List.filter ((/=) 0)
                |> List.map (dot base color)

        hours { fatal, serious, hour } =
            g [ transform [ Translate 0 (toFloat hour * hourHeight) ] ]
                (rect
                    [ width dayWidth
                    , height hourHeight
                    , x 0
                    , y 0
                    , fill FillNone
                    , stroke Color.grey
                    , strokeWidth 0.5
                    ]
                    []
                    :: (dotList 0 fatal (rgb255 139 0 0) ++ dotList fatal serious (rgb255 222 63 83))
                )
    in
    svg [ viewBox 0 0 w h, class [ "xmas-grid" ] ] (List.indexedMap xmas xmases)
