{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes  #-}

module Types where

import           Database.PostgreSQL.Simple
import           Data.Text                                ( Text )
import           Data.Time
import           GHC.Generics
import           Data.Csv                      as Csv
import           Data.Proxy
import qualified Data.ByteString.Char8         as BC8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Elm.Derive                    as Elm
import           Elm.Module
import Elm.Versions
import           Control.Lens
import Data.String.Interpolate

import TypeTH

data Rainfall
    = Rainfall
    { _location :: Text
    , _rain     :: Double
    , _day      :: Day
    } deriving (Show, Eq, Generic, FromRow)

data ScrollySection
    = ScrollySection
    { _label :: Text
    , _text :: Text
    } deriving (Show, Eq)

data ScrollyArticle
    = ScrollyArticle
    { _intro :: [ScrollySection]
    , _sections :: [ScrollySection]
    , _outro :: [ScrollySection] 
    } deriving (Show, Eq)


data Crash
    = Crash
    { _time :: UTCTime
    , _fatal :: Double
    , _minor :: Double
    , _nonInjury :: Double
    , _serious :: Double
    } deriving (Show, Eq, Generic, Csv.FromRecord)

instance FromField UTCTime where
    parseField f = 
        case parseTimeM True defaultTimeLocale "%F" (T.unpack . T.decodeUtf8 $ f) of
            Just utc -> return utc
            Nothing -> parseTimeM True defaultTimeLocale "%FT%T%Z" (T.unpack . T.decodeUtf8 $ f)

instance ToNamedRecord Rainfall where
    toNamedRecord = genericToNamedRecord csvOptions

instance FromNamedRecord Rainfall where
    parseNamedRecord = genericParseNamedRecord csvOptions

instance DefaultOrdered Rainfall where
    headerOrder = genericHeaderOrder csvOptions

instance FromField Day where
    parseField = parseTimeM True defaultTimeLocale "%Y-%m-%d" . BC8.unpack

instance ToField Day where
  toField = BC8.pack . formatTime defaultTimeLocale "%Y-%m-%d"

gen = [i|
#{head}

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

#{body}
|] 
    where
        head = moduleHeader Elm0p18 "App.DataTypes"
        body = makeModuleContent
            [ DefineElm (Proxy :: Proxy ScrollySection)
            , DefineElm (Proxy :: Proxy ScrollyArticle)
            , DefineElm (Proxy :: Proxy Crash)
            ]

deriveBoth elmOptions ''Rainfall
deriveBoth elmOptions ''ScrollySection
deriveBoth elmOptions ''ScrollyArticle
deriveBoth elmOptions ''Crash
makeFieldsNoPrefix ''Rainfall
makeFieldsNoPrefix ''ScrollySection
makeFieldsNoPrefix ''ScrollyArticle
makeFieldsNoPrefix ''Crash
