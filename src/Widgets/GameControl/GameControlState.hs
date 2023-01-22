module Widgets.GameControl.GameControlState
    ( GameControlState(..)
    , initState
    , mergeState
    ) where

import Control.Lens
import Data.Default
import Monomer
import qualified Monomer.Lens as L

import Util
import Widgets.GameControl.GameControlConfig
import Widgets.GameControl.GameControlData

data GameControlState = GameControlState
    { _gcsFixedRect :: Rect
    , _gcsOldFixedRect :: Rect
    , _gcsRunning :: Bool
    , _gcsStart :: Millisecond
    } deriving (Eq, Show)

instance Default GameControlState where
    def = GameControlState
        { _gcsFixedRect = Rect 0 0 0 0
        , _gcsOldFixedRect = Rect 0 0 0 0
        , _gcsRunning = False
        , _gcsStart = 0
        }

initState
    :: (GameControlConfig b a c)
    => GameControlData s a b
    -> WidgetEnv s e
    -> GameControlState
initState gcData wenv = def
    { _gcsFixedRect = getFixedRect gcData wenv
    , _gcsOldFixedRect = getFixedRect gcData wenv
    }

mergeState
    :: (GameControlConfig b a c)
    => GameControlState
    -> WidgetEnv s e
    -> GameControlData s a b
    -> WidgetNode s e
    -> (GameControlState, [WidgetRequest s e])
mergeState oldState wenv gcData newNode = (newState, reqs) where
    newState = if sameFixedRect
        then oldState
        else GameControlState
            { _gcsFixedRect = newFixedRect
            , _gcsOldFixedRect = oldFixedRect'
            , _gcsRunning = animationDuration' > 0
            , _gcsStart = ts
            }
    reqs = (ResizeWidgets widgetId):[reReq | not sameFixedRect]
    reReq = requestRenderEvery newNode animationDuration
    sameFixedRect = newFixedRect == oldFixedRect
    newFixedRect = getFixedRect gcData wenv
    oldFixedRect = _gcsFixedRect oldState
    olderFixedRect = _gcsOldFixedRect oldState
    oldFixedRect' = if delta < animationDuration
        then getMiddleRect olderFixedRect oldFixedRect progress
        else oldFixedRect
    oldStart = _gcsStart oldState
    delta = fromIntegral $ ts-oldStart
    progress = delta/animationDuration
    widgetId = newNode ^. L.info . L.widgetId
    animationDuration' = floor animationDuration :: Integer
    animationDuration = getAnimationDuration $ _gcdConfig gcData
    ts = wenv ^. L.timestamp
