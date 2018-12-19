{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Lib
    ( genElm
    , echoElm
    , dummyData
    , rainfall
    , article
    , Annual(..)
    ) where

import           Data.Aeson                      as A
import           Data.Aeson.Types                as A 
import qualified Data.ByteString.Lazy             as BL
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
import Data.Csv as Csv
import           Data.Proxy

import Scrolly
import Types

genElm :: FilePath -> IO ()
genElm fp =
    writeFile fp gen

echoElm :: IO ()
echoElm = putStrLn gen

dummyData :: FilePath -> IO ()
dummyData fp = BL.writeFile fp $ A.encode $ Rainfall "Hello" 9 (read "2015-12-24")



svg :: Element -> Text -> Double -> Double -> Element
svg content n x y = doctype <> with
    (svg11_ content)
    [ Version_ <<- "1.1"
    , Width_ <<- sformat float (2000)
    , Height_ <<- sformat float (ceiling $ y / x * 2000)
    , Id_ <<- n
    , ViewBox_ <<- sformat ("0 0 " % int % " " % int) (ceiling x) (ceiling y)
    ]

-- svgCoast (Coast w h g) =
--     let inner = g_ [Transform_ <<- sformat ("translate(0," % int % ")") (ceiling h)] (mconcat $ map path $ V.toList g)
--     in  svg inner "New Zealand" w h
--     where
--     path d = path_ [D_ <<- d, Fill_ <<- "none", Stroke_ <<- "#333", Stroke_width_ <<- "2.5"]


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

