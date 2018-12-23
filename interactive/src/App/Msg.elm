module App.Msg exposing (Msg(..))

import App.DataTypes exposing (..)
import Browser.Dom as Dom
import Http

type Msg
    = DataLoad (Result Http.Error Int)
    | Scroll ( String, Int )
    | ShowDay XmasDay
    | Size Int Int
    | RootSize (Result Dom.Error Dom.Viewport)