module Widgets.GameControl.GameControlModel
    ( GameControlModel(..)
    , initGameControlModel
    ) where

data GameControlModel a = GameControlModel
    { _gcmControlledGame :: Maybe a
    , _gcmShakeNode :: Maybe (Int, Int)
    } deriving Eq

initGameControlModel :: GameControlModel a
initGameControlModel = GameControlModel
    { _gcmControlledGame = Nothing
    , _gcmShakeNode = Nothing
    }
