{-# LANGUAGE FunctionalDependencies #-}

module Widgets.GameControl.GameControlColorConfig
    ( GameControlColorConfig
    , getDefaultNodeColors
    , getVisualGrid
    ) where

import Model.Grid
import Widgets.GameControlNode.NodeVisual
import Widgets.GameControlLink.LinkVisual

class GameControlColorConfig a b c | a -> b c where
    getDefaultNodeColors :: a -> c
    getVisualGrid :: b -> a -> Grid NodeVisual LinkVisual
