module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import DataTypes exposing (..)
import Html exposing (Html, div, h1, img, p, section, text)
import Html.Attributes exposing (attribute, class, id, src)
import Http
import Markdown exposing (toHtml)



---- MODEL ----


type alias Model =
    { article : List ScrollySection
    }


type alias Config =
    { article : List ScrollySection }


init : Config -> ( Model, Cmd Msg )
init { article } =
    ( Model article, Cmd.none )



---- UPDATE ----


type Msg
    = DataLoad (Result Http.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataLoad (Err e) ->
            ( model, Cmd.none )

        DataLoad (Ok f) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        step i scrolly =
            toHtml
                [ class "step"
                , attribute "data-label" scrolly.label
                , attribute "data-step" <| String.fromInt i
                ]
                scrolly.text
    in
    section [ id "scroll" ]
        [ div [ class "scroll__graphic sticky" ]
            [ div [ class "chart" ]
                [ p [] [ text "0" ]
                ]
            ]
        , div [ class "scroll__text" ]
            (List.indexedMap step model.article)
        ]



---- PROGRAM ----


main : Program Config Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
