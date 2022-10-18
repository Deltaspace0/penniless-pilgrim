module Model.Direction
    ( Direction(..)
    , getOpposite
    , nextPosition
    ) where

data Direction
    = North
    | South
    | West
    | East
    deriving (Eq, Show)

getOpposite :: Direction -> Direction
getOpposite North = South
getOpposite South = North
getOpposite West  = East
getOpposite East  = West

nextPosition :: Direction -> (Int, Int) -> (Int, Int)
nextPosition North (x, y) = (x, y-1)
nextPosition South (x, y) = (x, y+1)
nextPosition West  (x, y) = (x-1, y)
nextPosition East  (x, y) = (x+1, y)