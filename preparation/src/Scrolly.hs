{-# LANGUAGE OverloadedStrings #-}
module Scrolly where

import           Data.Text                                ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Text.Pandoc
import           Elm.Derive                    as Elm
import           Elm.Module
import           Control.Monad.IO.Class
import           Data.MonoTraversable                     ( onotElem )
import           Data.Char                                ( isSpace )
import Control.Lens
import Data.List (partition)
import Data.Maybe (mapMaybe)

import           Types

slugify :: Text -> Text
slugify =
    T.filter (not . isSpace)
        . T.intercalate "-"
        . T.words
        . T.replace "&" "and"
        . T.replace "+" "plus"
        . T.replace "%" "percent"
        . T.replace "<" "lt"
        . T.replace ">" "gt"
        . T.replace "=" "eq"
        . T.replace "#" "hash"
        . T.replace "@" "at"
        . T.replace "$" "dollar"
        . T.filter (`onotElem` ("!^*?()[]{}`./\\'\"~|" :: String))
        . T.toLower
        . T.strip

metaToString :: String -> Meta -> PandocIO [Text]
metaToString key meta = do
    case lookupMeta key meta of
        Just (MetaList ml) -> mapM wrt ml
        _ -> return []
    
    where
        wrt (MetaInlines inl) = writePlain def $ Pandoc nullMeta [Plain inl]
        wrt _ = return ""

article :: FilePath -> IO ScrollyArticle
article file = do
    t <- T.readFile file
    runIOorExplode $ do
        (Pandoc meta blks) <- readMarkdown
            def { readerExtensions = enableExtension Ext_raw_html pandocExtensions }
            t
        let (_, _, rv) = go blks (Nothing, [], [])
        allSections <- reverse <$> mapM (\(t,pd) -> do
            mt <- writeMarkdown def { writerExtensions = enableExtension Ext_raw_html pandocExtensions} $ Pandoc nullMeta $ reverse pd
            lt <- writePlain def  $ Pandoc nullMeta [Plain t]
            return $ ScrollySection (slugify lt) mt
            ) rv
        introSections <- metaToString "intro" meta
        outroSections <- metaToString "outro" meta
        let (intro,xs) = (part introSections) allSections
            (outro, body) = (part outroSections) xs
        return $ ScrollyArticle intro body outro
  where
    part sections = partition (\(ScrollySection l _) -> any (\y -> slugify y == l) sections)
    go (HorizontalRule : Header 1 _ label : xs) (Just t, bout, out) =
        let pd = (t, bout)
        in  go xs (Just label, [], pd : out)
    go (HorizontalRule : Header 1 _ label : xs) (Nothing, bout, out) =
        go xs (Just label, [], out)
    go []       (Just t, bout, out) = 
        let pd = (t, bout)
        in (Nothing, [], pd : out)
    go []       out                 = out
    go (h : xs) (Just t, bout, out) = go xs (Just t, h : bout, out)
    go (h : xs) (Nothing, bout, out) = go xs (Nothing, bout, out)
