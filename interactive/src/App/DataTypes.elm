
module App.DataTypes exposing(..)

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



type alias Xmas  =
   { year: Int
   , days: (List XmasDay)
   }

jsonDecXmas : Json.Decode.Decoder ( Xmas )
jsonDecXmas =
   Json.Decode.succeed (\pyear pdays -> {year = pyear, days = pdays})
   |> required "year" (Json.Decode.int)
   |> required "days" (Json.Decode.list (jsonDecXmasDay))

jsonEncXmas : Xmas -> Value
jsonEncXmas  val =
   Json.Encode.object
   [ ("year", Json.Encode.int val.year)
   , ("days", (Json.Encode.list jsonEncXmasDay) val.days)
   ]



type alias XmasDay  =
   { day: String
   , hours: (List XmasHour)
   }

jsonDecXmasDay : Json.Decode.Decoder ( XmasDay )
jsonDecXmasDay =
   Json.Decode.succeed (\pday phours -> {day = pday, hours = phours})
   |> required "day" (Json.Decode.string)
   |> required "hours" (Json.Decode.list (jsonDecXmasHour))

jsonEncXmasDay : XmasDay -> Value
jsonEncXmasDay  val =
   Json.Encode.object
   [ ("day", Json.Encode.string val.day)
   , ("hours", (Json.Encode.list jsonEncXmasHour) val.hours)
   ]



type alias XmasHour  =
   { hour: Int
   , fatal: (Maybe Int)
   , serious: (Maybe Int)
   }

jsonDecXmasHour : Json.Decode.Decoder ( XmasHour )
jsonDecXmasHour =
   Json.Decode.succeed (\phour pfatal pserious -> {hour = phour, fatal = pfatal, serious = pserious})
   |> required "hour" (Json.Decode.int)
   |> fnullable "fatal" (Json.Decode.int)
   |> fnullable "serious" (Json.Decode.int)

jsonEncXmasHour : XmasHour -> Value
jsonEncXmasHour  val =
   Json.Encode.object
   [ ("hour", Json.Encode.int val.hour)
   , ("fatal", (maybeEncode (Json.Encode.int)) val.fatal)
   , ("serious", (maybeEncode (Json.Encode.int)) val.serious)
   ]


