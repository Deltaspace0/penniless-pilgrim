{-# LANGUAGE FunctionalDependencies #-}

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

class (GameControlColorConfig b c) =>
    GameControlConfig a b c | a -> b c where
        getColorConfig :: a -> b
        getAnimationDuration :: a -> Double
        getLinkToNodeRatio :: a -> Double
        getNodeToWidthRatio :: a -> Double
        getWidth :: a -> Double
        getHeight :: a -> Double
