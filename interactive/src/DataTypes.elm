module DataTypes exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Rainfall  =
   { location: String
   , rain: Float
   , day: Day
   }

jsonDecRainfall : Json.Decode.Decoder ( Rainfall )
jsonDecRainfall =
   Json.Decode.succeed (\plocation prain pday -> {location = plocation, rain = prain, day = pday})
   |> required "location" (Json.Decode.string)
   |> required "rain" (Json.Decode.float)
   |> required "day" (jsonDecDay)

jsonEncRainfall : Rainfall -> Value
jsonEncRainfall  val =
   Json.Encode.object
   [ ("location", Json.Encode.string val.location)
   , ("rain", Json.Encode.float val.rain)
   , ("day", jsonEncDay val.day)
   ]



type alias ScrollySection  =
   { label: String
   , text: String
   }

jsonDecScrollySection : Json.Decode.Decoder ( ScrollySection )
jsonDecScrollySection =
   Json.Decode.succeed (\plabel ptext -> {label = plabel, text = ptext})
   |> required "label" (Json.Decode.string)
   |> required "text" (Json.Decode.string)

jsonEncScrollySection : ScrollySection -> Value
jsonEncScrollySection  val =
   Json.Encode.object
   [ ("label", Json.Encode.string val.label)
   , ("text", Json.Encode.string val.text)
   ]

