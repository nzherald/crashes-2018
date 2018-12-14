{-# LANGUAGE OverloadedStrings #-}
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util

import           Data.Aeson
import qualified Data.ByteString.Lazy          as BL
import           Crypto.Hash.MD5                          ( hashlazy )
import qualified Data.ByteString.Base16        as B16
import qualified Data.ByteString.Char8         as BS
import qualified Data.Text.Lazy.IO             as TL
import           Database.PostgreSQL.Simple
import           Graphics.Svg                             ( prettyText )
import Data.Csv (encodeDefaultOrderedByName)

import           Rules
import           Lib
import           Config

webapp = "interactive/dist/embed.js"
webpackCli = "interactive/node_modules/.bin/webpack"
generatedElm = "interactive/src/DataTypes.elm"
playData = "interactive/static/play.json"
articleText = "interactive/src/article.json"

svgCoastFile = "assets/coast.svg"


main :: IO ()
main = do
    (Config bld dbName) <- readConfig

    let coastShp    = bld </> "nz-coastlines-topo-150k.shp"
        crashData   = bld </> "crash-data.csv"
        rainfallCsv = bld </> "rainfall-19602016.csv"
        getConn     = connectPostgreSQL $ BS.pack $ "dbname=" ++ dbName
    shakeArgs shakeOptions { shakeFiles = bld } $ do
        db     <- addOracleCache addDb
        shp    <- addOracleCache loadShp2PgSql
        psql   <- addOracleCache runPsqlFile
        psqlin <- addOracleCache runPsqlFileStdin

        let hasDb = db $ Db dbName

        want ["data", articleText, generatedElm]

        
        "data" </> "average-rainfall.csv" %> \out -> do
            need ["data"]
            liftIO $ do
                conn   <- getConn
                d <- rainfall conn
                BL.writeFile out $ encodeDefaultOrderedByName d

        articleText %> \out -> do
            let input = "text" </> "article.md"
            need [input]
            liftIO $ do
                articleScrolly <- article input
                BL.writeFile out $ encode articleScrolly


        "data" ~> do
            hasDb
            psqlin $ PsqlFileStdin
                (dbName, "data" </> "rainfall.sql", rainfallCsv, [])
            psqlin $ PsqlFileStdin
                (dbName, "data" </> "load-nzta-data.sql", crashData, [])

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
            need $ [webpackCli] ++ deps
            command_ [Cwd "interactive"] "npm" ["run", "build"]

        webpackCli %> \_ -> command_ [Cwd "interactive"] "npm" ["i"]

        generatedElm %> \out -> do
            deps <- getDirectoryFiles "" ["preparation//*.hs"]
            need deps
            liftIO $ genElm out

        playData %> \out -> do
            deps <- getDirectoryFiles "" ["preparation//*.hs"]
            need deps
            liftIO $ dummyData out

        rainfallCsv %> unzip bld "data/mfe-rainfall-19602016-CSV.zip"

        -- svgCoastFile %> \out -> do
        --     deps <- getDirectoryFiles "" ["preparation//*.hs"]
        --     need $ "gis" : deps
        --     liftIO $ do
        --         conn   <- getConn
        --         coastD <- coast conn
        --         TL.writeFile out $ prettyText $ svgCoast $ coastD
  where
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
