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

import           Rules
import           Lib
import           Config

webapp = "interactive/dist/embed.js"
webpackCli = "interactive/node_modules/.bin/webpack"
generatedElm = "interactive/src/DataTypes.elm"
playData = "interactive/static/play.json"

svgCoastFile = "assets/coast.svg"


main :: IO ()
main = do
    (Config bld dbName) <- readConfig
    let coastShp = bld </> "nz-coastlines-topo-150k.shp"
        getConn  = connectPostgreSQL $ BS.pack $ "dbname=" ++ dbName
    shakeArgs shakeOptions { shakeFiles = bld } $ do
        db   <- addOracleCache addDb
        shp  <- addOracleCache loadShp2PgSql
        psql <- addOracleCache runPsqlFile

        let hasDb = db $ Db dbName

        want [webapp, generatedElm, playData, "gis", svgCoastFile]

        "gis" ~> do
            hasDb
            shp $ Shp2PgSql (dbName, "coast", coastShp, "2193")

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

        coastShp %> unzip bld "gis/kx-nz-coastlines-topo-150k-SHP.zip"

        svgCoastFile %> \out -> do
            deps <- getDirectoryFiles "" ["preparation//*.hs"]
            need $ "gis" : deps
            liftIO $ do
                conn   <- getConn
                coastD <- coast conn
                TL.writeFile out $ prettyText $ svgCoast $ coastD
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
