module Composites.GameControl.ControlledGame
    ( ControlledGame
    , getCurrentPosition
    , getScoreByPosition
    , getPreviousPositions
    , moveToDirection
    , moveToPosition
    ) where

import Common.Direction
import Data.Typeable

class (Eq a, Typeable a) => ControlledGame a where
    getCurrentPosition :: a -> (Int, Int)
    getScoreByPosition :: (Int, Int) -> a -> Maybe Double
    getPreviousPositions :: a -> [(Int, Int)]
    moveToDirection :: Direction -> a -> Maybe a
    moveToPosition :: (Int, Int) -> a -> Maybe a
