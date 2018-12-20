port module Main exposing (Model, Msg(..), init, main, update, view)

import BarChart exposing (barchart)
import Browser
import DataTypes exposing (..)
import Html exposing (Html, div, h1, iframe, img, p, section, text)
import Html.Attributes exposing (attribute, class, classList, height, id, src, style, width)
import Http
import Json.Encode as E
import Markdown exposing (toHtml)



---- MODEL ----


type alias Model =
    { article : ScrollyArticle
    , nym : List Annual
    , periods : List Annual
    , activeLabel : Maybe String
    , activeStep : Int
    }


type alias Config =
    { article : ScrollyArticle
    , nym : List Annual
    , periods : List Annual
    }


init : Config -> ( Model, Cmd Msg )
init { article, nym, periods } =
    ( Model article nym periods Nothing -1, Cmd.none )



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
                , barchart cls d
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
            [ barChart "fatal" "Fatal" <| List.map (\d -> ( d.year, d.fatal )) model.nym
            , barChart "serious" "Serious Injury" <| List.map (\d -> ( d.year, d.serious )) model.nym
            , barChart "minor" "Minor Injury" <| List.map (\d -> ( d.year, d.minor )) model.nym
            , barChart "non" "Non-injury" <| List.map (\d -> ( d.year, d.nonInjury )) model.nym
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
