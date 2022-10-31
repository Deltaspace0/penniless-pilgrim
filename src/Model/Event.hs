module Model.Event
    ( module Model.Direction
    , AppEvent(..)
    ) where

import Model.Direction

data AppEvent
    = AppInit
    | ResetPilgrim
    | ToggleConfig
    | ResizeGrid
    deriving (Eq, Show)