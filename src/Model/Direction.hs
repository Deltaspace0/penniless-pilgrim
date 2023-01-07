{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Direction
    ( Direction(..)
    , getOpposite
    , nextPosition
    , relativeDirection
    ) where

import Data.Aeson
import Data.List
import Data.Maybe
import Data.Text (pack)
import GHC.Generics

data Direction
    = North
    | South
    | West
    | East
    deriving (Eq, Show, Generic)

instance FromJSON Direction
instance ToJSON Direction

getOpposite :: Direction -> Direction
getOpposite North = South
getOpposite South = North
getOpposite West = East
getOpposite East = West

nextPosition :: Direction -> (Int, Int) -> (Int, Int)
nextPosition North (x, y) = (x, y-1)
nextPosition South (x, y) = (x, y+1)
nextPosition West (x, y) = (x-1, y)
nextPosition East (x, y) = (x+1, y)

relativeDirection :: (Int, Int) -> (Int, Int) -> Maybe Direction
relativeDirection (x, y) (x1, y1) = (directions!!) <$> ix where
    ix = elemIndex (x1, y1) positions
    directions = [North, South, West, East]
    positions = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]
