{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Lib
    ( genElm
    , echoElm
    , dummyData
    , svgCoast
    , coast
    ) where

import Elm.Derive
import Elm.Module
import Data.Aeson hiding (defaultOptions)
import Data.Aeson.Types hiding (defaultOptions)
import qualified Data.ByteString.Lazy as BL
import GHC.Generics

import           Graphics.Svg
import qualified Data.Text                     as T
import           Formatting
import qualified Data.Vector                   as V
import           Data.Text                                ( Text )
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.SqlQQ


import Data.Proxy

data Foo
   = Foo
   { f_name :: String
   , f_blablub :: Int
   } deriving (Show, Eq, Generic)

data Coast
   = Coast
   { co_width :: Double
   , co_height :: Double
   , co_geom :: V.Vector Text
   } deriving (Show, Eq, Generic, FromRow)

deriveBoth defaultOptions ''Foo

genElm :: FilePath -> IO ()
genElm fp =
    writeFile fp gen
    
echoElm :: IO ()
echoElm = putStrLn gen

dummyData :: FilePath -> IO ()
dummyData fp = BL.writeFile fp $ encode $ Foo "Hello" 9


gen = makeElmModule "DataTypes"
    [ DefineElm (Proxy :: Proxy Foo)
    ]

    
svg :: Element -> Text -> Double -> Double -> Element
svg content n x y = doctype <> with
    (svg11_ content)
    [ Version_ <<- "1.1"
    , Width_ <<- sformat float (2000)
    , Height_ <<- sformat float (ceiling $ y / x * 2000)
    , Id_ <<- n
    , ViewBox_ <<- sformat ("0 0 " % int % " " % int) (ceiling x) (ceiling y)
    ]

svgCoast (Coast w h g) =
    let inner = g_ [Transform_ <<- sformat ("translate(0," % int % ")") (ceiling h)] (mconcat $ map path $ V.toList g)
    in  svg inner "New Zealand" w h
    where
    path d = path_ [D_ <<- d, Fill_ <<- "none", Stroke_ <<- "#333", Stroke_width_ <<- "2.5"]



coast :: Connection -> IO Coast
coast conn = do
      c:_ <- query_ conn coastSql
      return c 
    
coastSql = [sql|
      WITH _e AS (
        SELECT st_extent(geom) as extent
        FROM coast
      ), _b AS (
        SELECT ceil(st_xmax(extent) - st_xmin(extent)) as width,
              ceil(st_ymax(extent) - st_ymin(extent)) as height
        FROM _e
      ), _g AS (
        SELECT ST_AsSvg(geom,1,2) as path
        FROM coast
      )
      SELECT width,
             height, 
             array_agg(path)
      FROM _b, _g
      GROUP BY width, height
    ;
    |]