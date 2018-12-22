{-# LANGUAGE OverloadedStrings #-}
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util

import Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy          as BL
import           Crypto.Hash.MD5                          ( hashlazy )
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text.Lazy.IO             as TL
import           Database.PostgreSQL.Simple
import           Graphics.Svg                             ( prettyText )
import           Data.Csv                                 ( encodeDefaultOrderedByName
                                                          )
import qualified Data.Csv                      as Csv
import qualified Data.Vector                   as V
import Data.List (groupBy)

import           Rules
import           Lib
import           Config

webapp = "interactive/dist/embed.js"
webpackCli = "interactive/node_modules/.bin/webpack"
generatedElm = "interactive/src/App/DataTypes.elm"
articleText = "interactive/src/article.json"


dataFiles = ("data" </> "hourly+xmas.csv") : map
    (\f -> "data" </> "crash+" ++ f)
    ["yearly.csv", "nym_yearly.csv", "xmas_periods.csv", "daily_trend.csv"]


main :: IO ()
main = do
    (Config bld dbName) <- readConfig

    let coastShp  = bld </> "nz-coastlines-topo-150k.shp"
        crashData = bld </> "crash-data.csv"
        getConn   = connectPostgreSQL $ BS.pack $ "dbname=" ++ dbName
    shakeArgs shakeOptions { shakeFiles = bld } $ do
        db     <- addOracleCache addDb
        shp    <- addOracleCache loadShp2PgSql
        psql   <- addOracleCache runPsqlFile
        psqlin <- addOracleCache runPsqlFileStdin

        let hasDb = db $ Db dbName
            dbLoad =
                psqlin $ PsqlFileStdin
                    (dbName, "data" </> "load-nzta-data.sql", crashData, [])

        want [webapp]


        "data" </> "average-rainfall.csv" %> \out -> do
            need ["data"]
            liftIO $ do
                conn <- getConn
                d    <- rainfall conn
                BL.writeFile out $ encodeDefaultOrderedByName d

        articleText %> \out -> do
            let input = "text" </> "article.md"
            need [input]
            liftIO $ do
                articleScrolly <- article input
                BL.writeFile out $ encode articleScrolly


        "data" ~> do
            hasDb
            dbLoad


        crashData %> \out -> do
            let raw = "data" </> "crashes by severity and hour.csv"
            need [raw]
            command_ [FileStdout out]
                     "iconv"
                     ["-f", "ISO-8859-2", "-t", "UTF-8", raw]

        phony "clean" $ do
            putNormal "Cleaning files in _build"
            removeFilesAfter "_build" ["//*"]

        webapp %> \out -> do
            deps <- getDirectoryFiles
                ""
                [ "interactive/src//*.js"
                , "interactive/src//*.json"
                , "interactive/src//*.elm"
                , "interactive/src//*.less"
                , "interactive/src//*.html"
                , "interactive/static/*"
                , "interactive/*.json"
                , "interactive/webpack.*"
                , "interactive/yarn.lock"
                ]
            need
                $  [ "data"
                   , articleText
                   , "interactive" </> "src" </> "crash+nym_yearly.json"
                   , "interactive" </> "src" </> "crash+xmas_periods.json"
                   , "interactive" </> "src" </> "crash+daily_trend.json"
                   , "interactive" </> "src" </> "hourly+xmas.json"
                   , generatedElm
                   , webpackCli
                   ]
                ++ deps
            command_ [Cwd "interactive"] "npm" ["run", "build"]

        webpackCli %> \out -> do
            command_ [Cwd "interactive"] "npm" ["i"]
            cmd_ $ "touch " ++ out

        generatedElm %> \out -> do
            deps <- getDirectoryFiles "" ["preparation//*.hs"]
            need deps
            liftIO $ genElm out

        dataFiles &%> \_ -> do
            hasDb
            dbLoad
            need ["data" </> "preprocess.R"]
            cmd_ ("Rscript data/preprocess.R" :: String)

        "interactive/src/crash+*.json" %> \out -> do
            let base = takeBaseName out
                src  = "data" </> addExtension base "csv"
            need [src]
            liftIO $ do
                bs <- BL.readFile src
                let ll = case Csv.decode Csv.HasHeader bs of
                        Right csv -> V.toList csv
                        Left  csv -> []
                BL.writeFile out $ encode (ll :: [Crash])

        "interactive/src/hourly+*.json" %> \out -> do
            let base = takeBaseName out
                src  = "data" </> addExtension base "csv"
            need [src]
            liftIO $ do
                h <- BL.readFile "data/hourly+xmas.csv"
                let (Right d) = Csv.decode Csv.HasHeader h
                    dd = (V.toList d::[HourRaw])
                    g = groupBy (\a b -> a ^. xmasYear  == b ^. xmasYear) dd
                    gg = map (\y -> groupBy (\a b -> a ^. day == b ^.day) y) g
                BL.writeFile out $ encode $ map xmas gg
  where
    xmas :: [[HourRaw]] -> Xmas
    xmas hr = Xmas (hr ^?! _head . _head . xmasYear) $ map xmasDay hr
    xmasDay :: [HourRaw] -> XmasDay
    xmasDay hr = XmasDay (hr ^?! _head . day) $ map xmasHour hr
    xmasHour (HourRaw _ _ h f s) = XmasHour h f s 
    unzip bld z o = do
        need [z]
        command_ [] "unzip" ["-o", z, "-d", bld]
        cmd_ $ "touch " ++ o


hashedWrite :: ToJSON a => [Char] -> a -> IO [Char]
hashedWrite pref d = do
    let bs = encode d
        hs = (pref ++ "-" ++ (BS.unpack $ B16.encode $ hashlazy bs) ++ ".json")
    BL.writeFile ("interactive" </> "static" </> hs) bs
    return hs
