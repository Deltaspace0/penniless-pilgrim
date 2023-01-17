module Widgets.GameControl.GameControlConfig
    ( module Widgets.GameControl.GameControlColorConfig
    , GameControlConfig
    , getColorConfig
    , getAnimationDuration
    , getLinkToNodeRatio
    , getNodeToWidthRatio
    , getWidth
    , getHeight
    ) where

import Widgets.GameControl.GameControlColorConfig

class GameControlConfig a where
    getColorConfig :: a -> GameControlColorConfig
    getAnimationDuration :: a -> Double
    getLinkToNodeRatio :: a -> Double
    getNodeToWidthRatio :: a -> Double
    getWidth :: a -> Double
    getHeight :: a -> Double
