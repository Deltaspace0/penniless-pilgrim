module Model.Pilgrim
    ( module Model.Direction
    , Pilgrim(..)
    , updateTax
    , isBack
    , updatePilgrim
    ) where

import Data.Default

import Model.Direction

data Pilgrim = Pilgrim
    { _position :: (Int, Int)
    , _tax :: Double
    , _path :: [Direction]
    } deriving (Eq, Show)

instance Default Pilgrim where
    def = Pilgrim
        { _position = (0, 0)
        , _tax = 0
        , _path = []
        }

updateTax :: Direction -> Double -> Double
updateTax North t = t/2
updateTax South t = t*2
updateTax West t = t-2
updateTax East t = t+2

isBack :: Direction -> Pilgrim -> Bool
isBack d pilgrim = not (null path) && d == opposite where
    path = _path pilgrim
    opposite = getOpposite $ head path

updatePilgrim :: Direction -> Pilgrim -> Pilgrim
updatePilgrim d pilgrim = Pilgrim
    { _position = nextPosition d $ _position pilgrim
    , _tax = updateTax d $ _tax pilgrim
    , _path = if isBack d pilgrim then tail path else (d:path)
    } where
        path = _path pilgrim