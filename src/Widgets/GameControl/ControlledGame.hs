module Widgets.GameControl.ControlledGame
    ( ControlledGame
    , getCurrentPosition
    , getGameGrid
    , getScoreByPosition
    , moveToDirection
    , moveToPosition
    ) where

import Model.Direction
import Model.Grid
import Model.Game.GameLink
import Model.Game.GameNode

class ControlledGame a where
    getCurrentPosition :: a -> (Int, Int)
    getGameGrid :: a -> Grid GameNode GameLink
    getScoreByPosition :: (Int, Int) -> a -> Maybe Double
    moveToDirection :: Direction -> a -> Maybe a
    moveToPosition :: (Int, Int) -> a -> Maybe a
