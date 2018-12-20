port module Main exposing (Model, Msg(..), init, main, update, view)

import BarChart exposing (..)
import Browser
import DataTypes exposing (..)
import Html exposing (Html, div, h1, iframe, img, p, section, text)
import Html.Attributes exposing (attribute, class, classList, height, id, src, style, width)
import Http
import Json.Encode as E
import Json.Decode exposing (Value, decodeValue, list)
import Markdown exposing (toHtml)
import Time exposing (Posix)
import DateFormat



---- MODEL ----


type alias Model =
    { article : ScrollyArticle
    , nym : List Crash
    , periods : List Crash
    , activeLabel : Maybe String
    , activeStep : Int
    }


type alias Config =
    { article : ScrollyArticle
    , nym : Value
    , periods : Value
    }


init : Config -> ( Model, Cmd Msg )
init { article, nym, periods } =
    let 
        dec v = decodeValue (list jsonDecCrash) v
                |> Result.withDefault []
    in
        ( Model article (dec nym) (dec periods) Nothing -1, Cmd.none )



---- UPDATE ----


type Msg
    = DataLoad (Result Http.Error Int)
    | Scroll ( String, Int )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataLoad (Err e) ->
            ( model, Cmd.none )

        DataLoad (Ok f) ->
            ( model, Cmd.none )

        Scroll ( si, i ) ->
            ( { model | activeStep = i, activeLabel = Just si }, Cmd.none )



---- VIEW ----


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
                    ]
                ]
            , div [ class "scroll__text" ]
                (model |> .article |> .sections |> List.indexedMap step)
            ]
        , section [ id "outro" ] (List.map (\s -> toHtml [] s.text) <| .outro <| .article <| model)
        ]


nymBarcharts model visible =
    let
        barChart cls label d =
            div [ class "barchart" ]
                [ div [ class "label" ] [ text label ]
                , barchart <| BarchartOptions d cls "Year" dateFormat 3
                ]
    in
    div [class "step-chart"
            , style "opacity" <|
                if visible then
                    "1"

                else
                    "0"]
        [ div
            [ class "chart-title" ]
            [ text "New Year's Morning Crashes" ]
        ,  div
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
                , barchart <| BarchartOptions d cls "Year" dayFormat 9 
                ]
    in
    div [class "step-chart"
            , style "opacity" <|
                if visible then
                    "1"

                else
                    "0"]
        [ div
            [ class "chart-title" ]
            [ text "Christmas Holiday Crashes" ]
        ,  div
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
    Sub.batch [ scroll Scroll ]


port scroll : (( String, Int ) -> msg) -> Sub msg



dayFormat : Posix -> String
dayFormat =
    DateFormat.format [ DateFormat.dayOfMonthFixed ] Time.utc

dateFormat : Posix -> String
dateFormat =
    DateFormat.format [ DateFormat.yearNumberLastTwo ] Time.utc