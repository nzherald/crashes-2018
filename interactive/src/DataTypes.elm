module DataTypes exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)


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



type alias ScrollyArticle  =
   { intro: (List ScrollySection)
   , sections: (List ScrollySection)
   , outro: (List ScrollySection)
   }

jsonDecScrollyArticle : Json.Decode.Decoder ( ScrollyArticle )
jsonDecScrollyArticle =
   Json.Decode.succeed (\pintro psections poutro -> {intro = pintro, sections = psections, outro = poutro})
   |> required "intro" (Json.Decode.list (jsonDecScrollySection))
   |> required "sections" (Json.Decode.list (jsonDecScrollySection))
   |> required "outro" (Json.Decode.list (jsonDecScrollySection))

jsonEncScrollyArticle : ScrollyArticle -> Value
jsonEncScrollyArticle  val =
   Json.Encode.object
   [ ("intro", (Json.Encode.list jsonEncScrollySection) val.intro)
   , ("sections", (Json.Encode.list jsonEncScrollySection) val.sections)
   , ("outro", (Json.Encode.list jsonEncScrollySection) val.outro)
   ]

