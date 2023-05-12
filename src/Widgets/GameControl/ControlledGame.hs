module Widgets.GameControl.ControlledGame
    ( ControlledGame
    , getCurrentPosition
    , getScoreByPosition
    , moveToDirection
    , moveToPosition
    ) where

import Common.Direction

class ControlledGame a where
    getCurrentPosition :: a -> (Int, Int)
    getScoreByPosition :: (Int, Int) -> a -> Maybe Double
    moveToDirection :: Direction -> a -> Maybe a
    moveToPosition :: (Int, Int) -> a -> Maybe a
