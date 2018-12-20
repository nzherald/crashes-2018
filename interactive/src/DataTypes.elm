
module DataTypes exposing(..)

import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set exposing (Set)
import Time exposing (Posix)
import Json.Decode.Extra exposing (datetime)
import Iso8601 exposing (fromTime)

jsonDecPosix = datetime
jsonEncPosix = Json.Encode.string << fromTime 

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



type alias Crash  =
   { time: Posix
   , fatal: Float
   , minor: Float
   , nonInjury: Float
   , serious: Float
   }

jsonDecCrash : Json.Decode.Decoder ( Crash )
jsonDecCrash =
   Json.Decode.succeed (\ptime pfatal pminor pnonInjury pserious -> {time = ptime, fatal = pfatal, minor = pminor, nonInjury = pnonInjury, serious = pserious})
   |> required "time" (jsonDecPosix)
   |> required "fatal" (Json.Decode.float)
   |> required "minor" (Json.Decode.float)
   |> required "nonInjury" (Json.Decode.float)
   |> required "serious" (Json.Decode.float)

jsonEncCrash : Crash -> Value
jsonEncCrash  val =
   Json.Encode.object
   [ ("time", jsonEncPosix val.time)
   , ("fatal", Json.Encode.float val.fatal)
   , ("minor", Json.Encode.float val.minor)
   , ("nonInjury", Json.Encode.float val.nonInjury)
   , ("serious", Json.Encode.float val.serious)
   ]


