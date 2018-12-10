module DataTypes exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


type alias Foo  =
   { f_name: String
   , f_blablub: Int
   }

jsonDecFoo : Json.Decode.Decoder ( Foo )
jsonDecFoo =
   Json.Decode.succeed (\pf_name pf_blablub -> {f_name = pf_name, f_blablub = pf_blablub})
   |> required "f_name" (Json.Decode.string)
   |> required "f_blablub" (Json.Decode.int)

jsonEncFoo : Foo -> Value
jsonEncFoo  val =
   Json.Encode.object
   [ ("f_name", Json.Encode.string val.f_name)
   , ("f_blablub", Json.Encode.int val.f_blablub)
   ]

