{-# LANGUAGE OverloadedStrings #-}

module Model.Pilgrim
    ( module Model.Direction
    , Pilgrim(..)
    , updateTax
    , isBack
    , updatePilgrim
    ) where

import Data.Aeson
import Data.Default

import Model.Direction

data Pilgrim = Pilgrim
    { _position :: (Int, Int)
    , _previousPositions :: [(Int, Int)]
    , _path :: [Direction]
    , _tax :: Double
    } deriving (Eq, Show)

instance Default Pilgrim where
    def = Pilgrim
        { _position = (0, 0)
        , _previousPositions = []
        , _path = []
        , _tax = 0
        }

instance FromJSON Pilgrim where
    parseJSON = withObject "Pilgrim" $ \v -> Pilgrim
        <$> v .: "position"
        <*> v .: "previous_positions"
        <*> v .: "path"
        <*> v .: "tax"

instance ToJSON Pilgrim where
    toJSON pilgrim = object
        [ "position" .= _position pilgrim
        , "previous_positions" .= _previousPositions pilgrim
        , "path" .= _path pilgrim
        , "tax" .= _tax pilgrim
        ]

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
    , _previousPositions = if goingBack
        then tail previousPositions
        else (p:previousPositions)
    , _path = if goingBack
        then tail path
        else (d:path)
    , _tax = updateTax d $ _tax pilgrim
    } where
        goingBack = isBack d pilgrim
        p = _position pilgrim
        previousPositions = _previousPositions pilgrim
        path = _path pilgrim