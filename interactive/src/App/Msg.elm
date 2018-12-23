module App.Msg exposing (Msg(..))

import App.DataTypes exposing (..)
import Browser.Dom as Dom
import Http

type Msg
    = Scroll ( String, Int )
    | ShowDay (Int, XmasDay)
    | Size Int Int
    | RootSize (Result Dom.Error Dom.Viewport)
    | CloseDetail
    | DataLoad (Result Http.Error (List Xmas))