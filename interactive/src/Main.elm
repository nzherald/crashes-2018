module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)
import Http

import DataTypes exposing (..)


---- MODEL ----


type alias Model =
    { dummy : Maybe Foo
    }


init : ( Model, Cmd Msg )
init =
    ( Model Nothing, getData )



---- UPDATE ----


type Msg
    = DataLoad (Result Http.Error Foo)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DataLoad (Err e) -> ( model, Cmd.none )
        DataLoad (Ok f) -> ({ model | dummy = Just f }, Cmd.none)
    



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , div [] [ text <| 
            case model.dummy of 
                    Nothing -> ""
                    Just d -> "Hello " ++ d.f_name
        ]
        ]


getData : Cmd Msg
getData =
  Http.send DataLoad (Http.get "play.json" jsonDecFoo)


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
