module Widgets.GameControlNode.ShakeState
    ( ShakeState(..)
    ) where

import Data.Default
import Monomer

data ShakeState = ShakeState
    { _ssRunning :: Bool
    , _ssStart :: Millisecond
    } deriving (Eq, Show)

instance Default ShakeState where
    def = ShakeState
        { _ssRunning = False
        , _ssStart = 0
        }