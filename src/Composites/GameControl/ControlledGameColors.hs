{-# LANGUAGE FunctionalDependencies #-}

module Composites.GameControl.ControlledGameColors
    ( ControlledGameColors
    , getVisualGrid
    ) where

import Common.Grid
import Composites.GameControl.ControlledGame
import Composites.GameControl.LinkVisual
import Composites.GameControl.NodeVisual
import Data.Typeable

class (ControlledGame a, Eq b, Typeable b)
    => ControlledGameColors a b | b -> a where
        getVisualGrid :: b -> a -> Grid NodeVisual LinkVisual
