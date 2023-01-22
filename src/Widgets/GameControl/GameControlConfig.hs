{-# LANGUAGE FunctionalDependencies #-}

module Widgets.GameControl.GameControlConfig
    ( module Widgets.GameControl.GameControlColorConfig
    , GameControlConfig
    , getAnimationDuration
    , getLinkToNodeRatio
    , getNodeToWidthRatio
    , getWidth
    , getHeight
    ) where

import Widgets.GameControl.GameControlColorConfig

class (GameControlColorConfig a b) => GameControlConfig a b where
    getAnimationDuration :: a -> Double
    getLinkToNodeRatio :: a -> Double
    getNodeToWidthRatio :: a -> Double
    getWidth :: a -> Double
    getHeight :: a -> Double
