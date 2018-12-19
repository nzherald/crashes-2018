{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Types where

import           Database.PostgreSQL.Simple
import           Data.Text                                ( Text )
import           Data.Time
import           GHC.Generics
import           Data.Csv                      as Csv
import           Data.Proxy
import qualified Data.ByteString.Char8         as BC8
import           Elm.Derive                    as Elm
import           Elm.Module
import           Control.Lens

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

gen = makeElmModule "DataTypes" 
    [ DefineElm (Proxy :: Proxy ScrollySection)
    , DefineElm (Proxy :: Proxy ScrollyArticle)
    ]

deriveBoth elmOptions ''Rainfall
deriveBoth elmOptions ''ScrollySection
deriveBoth elmOptions ''ScrollyArticle
makeFieldsNoPrefix ''Rainfall
makeFieldsNoPrefix ''ScrollySection
makeFieldsNoPrefix ''ScrollyArticle
