{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Direction
    ( Direction(..)
    , getOppositeDirection
    , getPositionInDirection
    , getRelativeDirection
    ) where

import Data.Aeson
import Data.List (elemIndex)
import GHC.Generics

data Direction
    = North
    | South
    | West
    | East
    deriving (Eq, Show, Generic)

instance FromJSON Direction
instance ToJSON Direction

getOppositeDirection :: Direction -> Direction
getOppositeDirection North = South
getOppositeDirection South = North
getOppositeDirection West = East
getOppositeDirection East = West

getPositionInDirection :: Direction -> (Int, Int) -> (Int, Int)
getPositionInDirection North (x, y) = (x, y-1)
getPositionInDirection South (x, y) = (x, y+1)
getPositionInDirection West (x, y) = (x-1, y)
getPositionInDirection East (x, y) = (x+1, y)

getRelativeDirection :: (Int, Int) -> (Int, Int) -> Maybe Direction
getRelativeDirection (x, y) (x1, y1) = (directions!!) <$> ix where
    ix = elemIndex (x1, y1) positions
    directions = [North, South, West, East]
    positions = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
