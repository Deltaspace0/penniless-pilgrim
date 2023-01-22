{-# LANGUAGE FunctionalDependencies #-}

module Widgets.GameControl.GameControlColorConfig
    ( GameControlColorConfig
    , getDefaultNodeColors
    , getVisualGrid
    ) where

import Model.Grid
import Widgets.GameControlNode.NodeColors
import Widgets.GameControlNode.NodeVisual
import Widgets.GameControlLink.LinkVisual

class GameControlColorConfig a b | a -> b where
    getDefaultNodeColors :: a -> NodeColors
    getVisualGrid :: b -> a -> Grid NodeVisual LinkVisual
