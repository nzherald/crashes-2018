module App.XmasGrid exposing (xmasGrid)

import App.DataTypes exposing (..)
import App.Msg exposing (..)
import Color exposing (rgb255)
import Html as H
import Html.Attributes as A
import Html.Events as E
import Numeral exposing (format)
import String.Extra exposing (toSentenceCase)
import String.Format exposing (namedValue, value)
import TypedSvg exposing (circle, g, rect, svg, text_)
import TypedSvg.Attributes
    exposing
        ( alignmentBaseline
        , class
        , fill
        , fontFamily
        , fontSize
        , fontVariant
        , stroke
        , textAnchor
        , transform
        , viewBox
        )
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, height, r, strokeWidth, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Events exposing (onClick)
import TypedSvg.Types
    exposing
        ( AlignmentBaseline(..)
        , AnchorAlignment(..)
        , Fill(..)
        , FontVariant(..)
        , Transform(..)
        )


margin =
    30


marginTop =
    70


xmasGrid w h xmases activeDay =
    let
        dayHeight =
            (h - marginTop) / (toFloat <| List.length xmases)

        dayWidth =
            if w > 700 then
                ((w * 0.5) - (2 * margin)) / 13

            else
                (w - (2 * margin)) / 13

        hourHeight =
            (dayHeight / 24) * 0.9

        xmas i x =
            g []
                [ g
                    [ transform
                        [ Translate 0 (toFloat i * dayHeight)
                        ]
                    ]
                    [ text_
                        [ transform
                            [ Rotate -90 0 0
                            , Translate (dayHeight * -0.45) 20
                            ]
                        , fontSize 16
                        , fontFamily [ "Stag Sans" ]
                        , textAnchor AnchorMiddle
                        ]
                        [ text <| String.fromInt x.year ]
                    ]
                , g [ transform [ Translate margin (toFloat i * dayHeight) ] ]
                    (List.indexedMap (days i) x.days)
                ]

        days i j d =
            g
                [ transform [ Translate (toFloat j * dayWidth) 0 ]
                , onClick <| ShowDay ( i, d )
                ]
                (rect
                    [ class [ "day" ]
                    , fill <|
                        Fill
                            (if Just ( i, d ) == activeDay then
                                rgb255 200 200 200

                             else
                                Color.white
                            )
                    , width dayWidth
                    , height (dayHeight * 0.9)
                    ]
                    []
                    :: List.map hours d.hours
                )

        dot base col i =
            let rr = (Basics.min (hourHeight * 0.45) ((dayWidth - hourHeight) / 9))
            in circle
                [ cx ((toFloat (base + (i-1))) * (rr * 2.2) + (0.5*hourHeight))
                , cy (hourHeight * 0.5)
                , r rr
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

        label i ( m, l ) =
            g [ transform [ Translate (toFloat i * dayWidth + 0.5 * dayWidth + margin) 0 ] ]
                [ text_ [ y 60, textAnchor AnchorMiddle, fontSize 16 ] [ text l ]
                , text_ [ y 40, textAnchor AnchorMiddle, fontSize 12, fontVariant FontVariantSmallCaps ] [ text m ]
                ]

        labels =
            g []
                (List.indexedMap label
                    [ ( "Dec", "24" )
                    , ( "Dec", "25" )
                    , ( "Dec", "26" )
                    , ( "Dec", "27" )
                    , ( "Dec", "28" )
                    , ( "Dec", "29" )
                    , ( "Dec", "30" )
                    , ( "Dec", "31" )
                    , ( "Jan", "1" )
                    , ( "Jan", "2" )
                    , ( "Jan", "3" )
                    , ( "Jan", "4" )
                    , ( "Jan", "5" )
                    ]
                )

        hourLabels i l =
            text_
                [ x -2
                , y (toFloat i * hourHeight)
                , textAnchor AnchorEnd
                , alignmentBaseline AlignmentCentral
                , fontSize 3.5
                , fontFamily [ "Stag Sans" ]
                ]
                [ text <| (format "00" <| toFloat l) ++ ":00" ]

        word i =
            case i of
                2 ->
                    "two"

                3 ->
                    "three"

                4 ->
                    "four"

                5 ->
                    "five"

                _ ->
                    String.fromInt i

        detail fatal serious =
            case ( fatal, serious ) of
                ( 0, 0 ) ->
                    ""

                ( 0, 1 ) ->
                    "one serious injury crash"

                ( 1, 0 ) ->
                    "one fatal crash"

                ( 0, _ ) ->
                    "{{ }} serious injury crashes" |> value (word serious)

                ( _, 0 ) ->
                    "{{ }} fatal crashes" |> value (word fatal)

                ( 1, 1 ) ->
                    "one fatal and one serious injury crash" |> value (word serious)

                ( 1, _ ) ->
                    "one fatal and {{ }} serious injury crashes" |> value (word serious)

                ( _, 1 ) ->
                    "{{ }} fatal and one serious injury crashes" |> value (word fatal)

                _ ->
                    "{{ fatal }} fatal and {{ serious }} serious injury crashes"
                        |> namedValue "fatal" (word fatal)
                        |> namedValue "serious" (word serious)

        hourDetail i { fatal, serious } =
            text_
                [ x (dayWidth * 1.1)
                , y (toFloat i * hourHeight + (hourHeight / 2))
                , alignmentBaseline AlignmentCentral
                , fontSize 4
                ]
                [ text <| toSentenceCase <| detail fatal serious ]

        highlight =
            case activeDay of
                Just ( i, active ) ->
                    let
                        fatal =
                            List.map .fatal active.hours |> List.sum

                        serious =
                            List.map .serious active.hours |> List.sum

                        dw =
                            if w > 700 then
                                w * 0.5

                            else
                                w * 0.7
                    in
                    H.div
                        [ A.style "position" "absolute"
                        , A.style "top"
                            (String.fromFloat
                                (Basics.max marginTop
                                    ((toFloat i * dayHeight + marginTop) - 0.5 * dayHeight)
                                )
                                ++ "px"
                            )
                        , A.style "right" "0"
                        ]
                        [ H.div
                            [ A.style "border" "1px solid #333"
                            , A.style "padding" "10px 20px"
                            , A.style "margin" "20px"
                            , A.style "max-width" (String.fromFloat dw ++ "px")
                            , A.style "min-width" "280px"
                            , A.style "background-color" "#F9F9F9"
                            , A.class "detail"
                            , E.onClick CloseDetail
                            ]
                            [ H.div [ A.class "close"]
                            [ ]
                            , H.div [ A.style "max-width" "400px" ]
                                [ "On {{ day }} a total of {{ detail }} occurred."
                                    |> namedValue "day" active.day
                                    |> namedValue "detail" (detail fatal serious)
                                    |> toSentenceCase
                                    |> text
                                ]
                            , svg
                                [ viewBox 0 0 (11.5 * dayWidth) (dayHeight * 3.1)
                                ]
                                [ g
                                    []
                                    [ g [ transform [ Translate 35 30, Scale 3 3 ] ]
                                        (List.map hours active.hours
                                            ++ (List.range 0 24 |> List.map (Basics.modBy 24) |> List.indexedMap hourLabels)
                                            ++ List.indexedMap hourDetail active.hours
                                        )
                                    ]
                                ]
                            ]
                        ]

                Nothing ->
                    H.div [] []
    in
    H.div
        [ A.style "position" "relative"
        , A.style "overflow" "visible"
        ]
        [ svg [ viewBox 0 0 w h, class [ "xmas-grid" ] ]
            [ g []
                [ labels
                , text_
                    [ y 10
                    , x (w * 0.34)
                    , textAnchor AnchorEnd
                    , fill <| Fill <| rgb255 131 0 0
                    , fontSize 13
                    , alignmentBaseline AlignmentCentral
                    ]
                    [ text "Fatal crash" ]
                , circle [ cx (w * 0.36), cy 10, r 5, fill <| Fill <| rgb255 131 0 0 ] []
                , text_
                    [ y 10
                    , x (w * 0.7)
                    , textAnchor AnchorEnd
                    , fill <| Fill <| rgb255 222 63 83
                    , fontSize 13
                    , alignmentBaseline AlignmentCentral
                    ]
                    [ text "Serious injury crash" ]
                , circle [ cx (w * 0.72), cy 10, r 5, fill <| Fill <| rgb255 222 63 83 ] []
                ]
            , g [ transform [ Translate 0 marginTop ] ] (List.indexedMap xmas xmases)
            ]
        , highlight
        ]
