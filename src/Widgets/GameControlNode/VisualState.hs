module Widgets.GameControlNode.VisualState
    ( VisualState(..)
    , isRunningVisualState
    ) where

import Data.Default
import Monomer

import Widgets.GameControlNode.NodeVisual

data VisualState = VisualState
    { _vsVisual :: Maybe NodeVisual
    , _vsStart :: Millisecond
    } deriving (Eq, Show)

instance Default VisualState where
    def = VisualState
        { _vsVisual = Nothing
        , _vsStart = 0
        }

isRunningVisualState :: Double -> Millisecond -> VisualState -> Bool
isRunningVisualState animationDuration ts visualState = r where
    r = ts-(_vsStart visualState) < floor animationDuration
