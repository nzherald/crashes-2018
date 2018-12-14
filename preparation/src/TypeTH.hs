module TypeTH where


import           Data.Csv                      as Csv
import           Elm.Derive                    as Elm

csvOptions :: Csv.Options
csvOptions = Csv.defaultOptions { Csv.fieldLabelModifier = rmUnderscore }

elmOptions = Elm.defaultOptions { Elm.fieldLabelModifier = rmUnderscore }

rmUnderscore :: String -> String
rmUnderscore ('_' : str) = str
rmUnderscore str         = str
    