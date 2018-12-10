{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}

module Rules where

import           Data.String.Interpolate
import           Development.Shake
import           Development.Shake.Classes
import           Development.Shake.Command

newtype PsqlFile =
  PsqlFile (String, FilePath, [FilePath])
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult PsqlFile = ()

newtype Db =
  Db String
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult Db = ()

newtype DbExcel =
  DbExcel (String, String, Int, Int)
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult DbExcel = ()

newtype Shp2PgSql =
  Shp2PgSql (String, String, String, String)
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult Shp2PgSql = ()

addDb (Db dbname) = do
  command_ [] "dropdb" ["--if-exists", dbname]
  command_ [] "createdb" [dbname]
  command_ [] "psql" ["-c", "CREATE EXTENSION postgis", dbname]

loadDbExcel (DbExcel (tbl, file, sheet, skip)) = do
  need [file]
  let script =
        [i|
            library(tidyverse)
            library(RPostgreSQL)
            drv <- dbDriver('PostgreSQL')
            con <- dbConnect(drv, dbname='property-report')
            d <- readxl::read_excel("#{file}", sheet = #{sheet}, skip = #{skip}) %>% rename_all(make.names)
            copy_to(con, d, name="#{tbl}", temporary=F, overwrite=T)
        |]
  command_ [Stdin script] "R" ["--vanilla"]

loadShp2PgSql (Shp2PgSql (dbname, table, file, srid)) = do
  need [file]
  Stdout out <-
    command [] "shp2pgsql" ["-d", "-I", "-D", "-s", srid, file, table]
  command [StdinBS out] "psql" [dbname]


runPsqlFile (PsqlFile (dbName, sql, dep)) = do
    need $ sql : dep
    command_ [] "psql" ["-f", sql, "-v", "ON_ERROR_STOP=1", dbName]
