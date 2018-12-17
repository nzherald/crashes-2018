module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text, section, p)
import Html.Attributes exposing (src, attribute, id, class)
import Http



-- import DataTypes exposing (..)
---- MODEL ----


type alias Model =
    { dummy : Maybe Int
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing, Cmd.none )



---- UPDATE ----


type Msg
    = DataLoad (Result Http.Error Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataLoad (Err e) ->
            ( model, Cmd.none )

        DataLoad (Ok f) ->
            ( { model | dummy = Just f }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    section [ id "scroll" ]
        [ div [ class "scroll__graphic sticky" ]
            [ div [ class "chart" ]
                [ p [] [ text "0" ]
                ]
            ]
        , div [ class "scroll__text" ]
            [ div [ class "step", attribute "data-step" "1" ]
                [ p [] [ text "STEP 1" ]
                ]
            , div [ class "step", attribute "data-step" "2" ]
                [ p [] [ text "STEP 2" ]
                ]
            , div [ class "step", attribute "data-step" "3" ]
                [ p [] [ text "STEP 3" ]
                ]
            , div [ class "step", attribute "data-step" "4" ]
                [ p [] [ text "STEP 4" ]
                ]
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
