module Model.Game.Pilgrim
    ( module Model.Direction
    , Pilgrim(..)
    , updateTax
    , isOppositeToPilgrim
    , updatePilgrim
    ) where

import Data.Aeson
import Data.Default

import Model.Direction

data Pilgrim = Pilgrim
    { _position :: (Int, Int)
    , _path :: [((Int, Int), Direction)]
    , _tax :: Double
    } deriving (Eq, Show)

instance Default Pilgrim where
    def = Pilgrim
        { _position = (0, 0)
        , _path = []
        , _tax = 0
        }

instance FromJSON Pilgrim where
    parseJSON = withObject "Pilgrim" $ \v -> Pilgrim
        <$> v .: "position"
        <*> v .: "path"
        <*> v .: "tax"

instance ToJSON Pilgrim where
    toJSON pilgrim = object
        [ "position" .= _position pilgrim
        , "path" .= _path pilgrim
        , "tax" .= _tax pilgrim
        ]

updateTax :: Direction -> Double -> Double
updateTax North t = t/2
updateTax South t = t*2
updateTax West t = t-2
updateTax East t = t+2

isOppositeToPilgrim :: Direction -> Pilgrim -> Bool
isOppositeToPilgrim d pilgrim = not (null path) && d == opp where
    path = _path pilgrim
    opp = getOppositeDirection $ snd $ head path

updatePilgrim :: Direction -> Pilgrim -> Pilgrim
updatePilgrim d pilgrim = Pilgrim
    { _position = getPositionInDirection d $ _position pilgrim
    , _path = if isOppositeToPilgrim d pilgrim
        then tail $ _path pilgrim
        else (_position pilgrim, d):(_path pilgrim)
    , _tax = updateTax d $ _tax pilgrim
    }
