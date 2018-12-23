port module Main exposing (Model, init, main, update, view)

import App.BarChart exposing (..)
import App.DataTypes exposing (..)
import App.LineChart exposing (..)
import App.Msg exposing (..)
import App.XmasGrid exposing (..)
import Browser
import Browser.Dom exposing (Viewport, getViewportOf)
import Browser.Events exposing (onResize)
import DateFormat
import Html exposing (Html, div, h1, iframe, img, p, section, text)
import Html.Attributes exposing (attribute, class, classList, height, id, src, style, width)
import Http
import Json.Decode exposing (Value, decodeValue, list)
import Json.Encode as E
import Markdown exposing (defaultOptions, toHtmlWith)
import Task
import Time exposing (Posix)



---- MODEL ----


type alias Model =
    { article : ScrollyArticle
    , nym : List Crash
    , periods : List Crash
    , trends : List Crash
    , hourly : List Xmas
    , activeLabel : Maybe String
    , activeStep : Int
    , small : Bool
    , width : Float
    , activeDay : Maybe ( Int, XmasDay )
    }


type alias Config =
    { article : ScrollyArticle
    , nym : Value
    , periods : Value
    , trends : Value
    , small : Bool
    , width : Float
    }


init : Config -> ( Model, Cmd Msg )
init { article, nym, periods, trends, small, width } =
    let
        dec v =
            decodeValue (list jsonDecCrash) v
                |> Result.withDefault []
    in
    ( Model article (dec nym) (dec periods) (dec trends) [] Nothing 0 small width Nothing, getData )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Scroll ( si, i ) ->
            ( { model | activeStep = i, activeLabel = Just si }, Cmd.none )

        ShowDay d ->
            ( { model | activeDay = Just d }, Cmd.none )

        Size _ _ ->
            ( model, Task.attempt RootSize (getViewportOf "root") )

        RootSize (Err _) ->
            ( model, Cmd.none )

        RootSize (Ok { viewport }) ->
            ( { model | width = viewport.width }, Cmd.none )

        CloseDetail ->
            ( { model | activeDay = Nothing }, Cmd.none )

        DataLoad (Err e) ->
            ( model, Cmd.none )

        DataLoad (Ok f) ->
            ( { model | hourly = f }, Cmd.none )



---- VIEW ----


toHtml =
    toHtmlWith { defaultOptions | sanitize = False }


view : Model -> Html Msg
view model =
    let
        step i scrolly =
            toHtml
                [ class "step"
                , classList [ ( "is-active", i == model.activeStep ) ]
                , attribute "data-label" scrolly.label
                , attribute "data-step" <| String.fromInt i
                ]
                scrolly.text
    in
    div []
        [ section [ id "intro" ] (List.map (\s -> toHtml [] s.text) <| .intro <| .article <| model)
        , section [ id "scroll" ]
            [ div [ class "scroll__graphic sticky" ]
                [ div [ class "chart" ]
                    [ nymBarcharts model (model.activeStep == 0)
                    , periodBarcharts model (model.activeStep == 1)
                    , trendCharts model (model.activeStep == 2 || model.activeStep == 3)
                    ]
                ]
            , div [ class "scroll__text" ]
                (model |> .article |> .sections |> List.indexedMap step)
            ]
        , section [ id "outro" ]
            [ div [] (List.map (\s -> toHtml [] s.text) <| .outro <| .article <| model)
            , div
                [ style "font-family" "Stag Sans"
                , style "text-align" "center"
                , style "margin" "10px auto"
                , style "font-size" "24px"
                ]
                [ text "Hourly Fatal and Serious Injury Crashes" ]
            , xmasGrid model.width 2000 model.hourly model.activeDay
            ]
        ]


nymBarcharts model visible =
    let
        barChart cls label d =
            div [ class "barchart" ]
                [ div [ class "label" ] [ text label ]
                , barchart <| BarchartOptions d cls "Year" dateFormat 3 model.small
                ]
    in
    div
        [ class "step-chart"
        , style "opacity" <|
            if visible then
                "1"

            else
                "0"
        , style "pointer-events" <|
            if visible then
                "inherit"

            else
                "none"
        ]
        [ div
            [ class "chart-title" ]
            [ text "New Year's Morning Crashes" ]
        , div
            [ class "chart-subtitle" ]
            [ text "Total number of crashes recorded between midnight and 6am on January 1 since 2000" ]
        , div
            [ class "quartet"
            ]
            [ barChart "fatal" "Fatal" <| List.map (\d -> ( d.time, d.fatal )) model.nym
            , barChart "serious" "Serious Injury" <| List.map (\d -> ( d.time, d.serious )) model.nym
            , barChart "minor" "Minor Injury" <| List.map (\d -> ( d.time, d.minor )) model.nym
            , barChart "non" "Non-injury" <| List.map (\d -> ( d.time, d.nonInjury )) model.nym
            ]
        ]


periodBarcharts model visible =
    let
        barChart cls label d =
            div [ class "barchart" ]
                [ div [ class "label" ] [ text label ]
                , barchart <| BarchartOptions d cls "Year" dayFormat 9 model.small
                ]
    in
    div
        [ class "step-chart"
        , id "period-chart"
        , style "opacity" <|
            if visible then
                "1"

            else
                "0"
        , style "pointer-events" <|
            if visible then
                "inherit"

            else
                "none"
        ]
        [ div
            [ class "chart-title" ]
            [ text "Christmas Holiday Crashes" ]
        , div
            [ class "chart-subtitle" ]
            [ text "Total number of crashes in each six hour period of the Christmas Holiday since 2000" ]
        , div
            [ class "quartet"
            ]
            [ barChart "fatal" "Fatal" <| List.map (\d -> ( d.time, d.fatal )) model.periods
            , barChart "serious" "Serious Injury" <| List.map (\d -> ( d.time, d.serious )) model.periods
            , barChart "minor" "Minor Injury" <| List.map (\d -> ( d.time, d.minor )) model.periods
            , barChart "non" "Non-injury" <| List.map (\d -> ( d.time, d.nonInjury )) model.periods
            ]
        ]


trendCharts model visible =
    let
        lineChart cls label d =
            div [ class "linechart" ]
                [ div [ class "label" ] [ text label ]
                , linechart model.small cls "Day" d
                ]
    in
    div
        [ class "step-chart"
        , style "opacity" <|
            if visible then
                "1"

            else
                "0"
        , style "pointer-events" <|
            if visible then
                "inherit"

            else
                "none"
        ]
        [ div
            [ class "chart-title" ]
            [ text "Daily Crashes" ]
        , div
            [ class "chart-subtitle" ]
            [ text "Average number of crashes that occur per day since Jan 1, 2000" ]
        , div
            [ class "quartet"
            ]
            [ lineChart "fatal" "Fatal" <| List.map (\d -> ( d.time, d.fatal )) model.trends
            , lineChart "serious" "Serious Injury" <| List.map (\d -> ( d.time, d.serious )) model.trends
            , lineChart "minor" "Minor Injury" <| List.map (\d -> ( d.time, d.minor )) model.trends
            , lineChart "non" "Non-injury" <| List.map (\d -> ( d.time, d.nonInjury )) model.trends
            ]
        ]



---- PROGRAM ----


main : Program Config Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ scroll Scroll, onResize Size ]


port scroll : (( String, Int ) -> msg) -> Sub msg


dayFormat : Posix -> String
dayFormat =
    DateFormat.format [ DateFormat.dayOfMonthFixed ] Time.utc


dateFormat : Posix -> String
dateFormat =
    DateFormat.format [ DateFormat.yearNumberLastTwo ] Time.utc


getData : Cmd Msg
getData =
    Http.send DataLoad (Http.get "https://insights.nzherald.co.nz/apps/2018/crashes-2018/hourly+xmas.json" (Json.Decode.list jsonDecXmas))
