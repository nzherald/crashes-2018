{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE QuasiQuotes #-}

module Config where

import           Data.Aeson
import           Data.Yaml
import           GHC.Generics
import Data.String.Here


readConfig :: IO Config
readConfig = do
    ec <- decodeFileEither "config.yaml"
    case ec of
        Left  _ -> error errorMsg
        Right c -> return c

errorMsg = [here|

The config file - which must be config.yaml has not been decoded.

The most likely reason for this is that the field database has not been
defined. You must define it - but be aware that if a database of that
name exists it will be destroyed.

|]


data Config = Config
    { build :: String
    , database :: String
    } deriving (Show, Eq, Generic, FromJSON)
