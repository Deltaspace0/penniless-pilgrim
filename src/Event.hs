module Event
    ( module Direction
    , AppEvent(..)
    ) where

import Direction

data AppEvent
    = AppInit
    | MovePilgrim Direction
    | ResetPilgrim
    | ToggleConfig
    | ResizeGrid
    deriving (Eq, Show)