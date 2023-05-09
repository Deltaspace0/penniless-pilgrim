{-# LANGUAGE FunctionalDependencies #-}

module Widgets.GameControl.GameControlColorConfig
    ( GameControlColorConfig
    , getDefaultNodeVisual
    , getVisualGrid
    ) where

import Model.Grid
import Widgets.GameControlNode.NodeVisual
import Widgets.GameControlLink.LinkVisual

class GameControlColorConfig a b | a -> b where
    getDefaultNodeVisual :: a -> NodeVisual
    getVisualGrid :: b -> a -> Grid NodeVisual LinkVisual
