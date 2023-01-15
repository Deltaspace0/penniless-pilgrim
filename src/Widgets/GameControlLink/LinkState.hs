module Widgets.GameControlLink.LinkState
    ( LinkState(..)
    , initState
    , mergeState
    ) where

import Data.Default
import Monomer

import Util
import Widgets.GameControlLink.LinkData
import Widgets.GameControlLink.LinkVisual

data LinkState = LinkState
    { _lsLink :: Maybe LinkVisual
    , _lsOldLink :: Maybe LinkVisual
    , _lsRunning :: Bool
    , _lsStart :: Millisecond
    } deriving (Eq, Show)

instance Default LinkState where
    def = LinkState
        { _lsLink = Nothing
        , _lsOldLink = Nothing
        , _lsRunning = False
        , _lsStart = 0
        }

initState :: LinkData -> LinkState
initState linkData = def
    { _lsLink = _ldLink linkData
    , _lsOldLink = _ldLink linkData
    }

mergeState
    :: LinkState
    -> Millisecond
    -> LinkData
    -> WidgetNode s e
    -> (LinkState, [WidgetRequest s e])
mergeState oldState ts linkData newNode = (newState, reqs) where
    newState = if newLink == oldLink
        then oldState
        else LinkState
            { _lsLink = newLink
            , _lsOldLink = oldLink
            , _lsRunning = animationDuration' > 0
            , _lsStart = newStart
            }
    reqs = if newLink == oldLink
        then []
        else [requestRenderEvery newNode animationDuration]
    newLink = _ldLink linkData
    oldLink = _lsLink oldState
    oldStart = _lsStart oldState
    delta = min animationDuration' (ts-oldStart)
    newStart = ts - animationDuration' + delta
    animationDuration' = floor animationDuration
    animationDuration = _ldAnimationDuration linkData
