module Widgets.GameControl.GameControlConfig
    ( module Widgets.GameControl.GameControlColorConfig
    , GameControlConfig(..)
    ) where

import Widgets.GameControl.GameControlColorConfig

data GameControlConfig = GameControlConfig
    { _gccColorConfig :: GameControlColorConfig
    , _gccAnimationDuration :: Double
    , _gccLinkToNodeRatio :: Double
    , _gccNodeToWidthRatio :: Double
    , _gccWidth :: Double
    , _gccHeight :: Double
    } deriving (Eq, Show)