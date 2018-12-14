{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib
    ( genElm
    , echoElm
    , dummyData
    , svgCoast
    , coast
    , rainfall
    ) where

import           Data.Aeson                      as A
import           Data.Aeson.Types                as A 
import qualified Data.ByteString.Lazy             as BL
import           Elm.Derive as Elm
import           Elm.Module
import           GHC.Generics

import           Control.Lens
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Time
import qualified Data.Vector                      as V
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.SqlQQ
import           Formatting
import           Graphics.Svg
import qualified Data.ByteString.Char8 as BC8
import Data.Csv as Csv

import           Data.Proxy

data Rainfall
   = Rainfall
   { _location :: Text
   , _rain     :: Double
   , _day      :: Day
   } deriving (Show, Eq, Generic, FromRow)

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

data Coast
   = Coast
   { co_width  :: Double
   , co_height :: Double
   , co_geom   :: V.Vector Text
   } deriving (Show, Eq, Generic, FromRow)



genElm :: FilePath -> IO ()
genElm fp =
    writeFile fp gen

echoElm :: IO ()
echoElm = putStrLn gen

dummyData :: FilePath -> IO ()
dummyData fp = BL.writeFile fp $ A.encode $ Rainfall "Hello" 9 (read "2015-12-24")


gen = makeElmModule "DataTypes"
    [ DefineElm (Proxy :: Proxy Rainfall)
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

rainfall :: Connection -> IO [Rainfall]
rainfall conn = query_ conn rainfallSql

rainfallSql = [sql|
SELECT 'New Zealand', avg(rain), day
FROM rainfall
WHERE ((date_part('month', day) = 12 AND date_part('day', day) >= 24) 
  OR (date_part('month', day) = 1 AND date_part('day', day) <= 5) )
  AND day >= '2001-12-24'
GROUP BY day

|]


csvOptions :: Csv.Options
csvOptions = Csv.defaultOptions { Csv.fieldLabelModifier = rmUnderscore }
  where
    rmUnderscore ('_':str) = str
    rmUnderscore str       = str




deriveBoth Elm.defaultOptions ''Rainfall
makeLenses ''Rainfall