{-# LANGUAGE RecordWildCards #-}

module Widgets.GameControlLink.LinkState
    ( LinkState(..)
    , initState
    , mergeState
    ) where

import Data.Default
import Monomer

import Common.Util
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
mergeState oldState ts LinkData{..} newNode = (newState, reqs) where
    newState = if _ldLink == oldLink
        then oldState
        else LinkState
            { _lsLink = _ldLink
            , _lsOldLink = oldLink
            , _lsRunning = animationDuration' > 0
            , _lsStart = newStart
            }
    reqs = if _ldLink == oldLink
        then []
        else [requestRenderEvery newNode _ldAnimationDuration]
    oldLink = _lsLink oldState
    oldStart = _lsStart oldState
    delta = min animationDuration' (ts-oldStart)
    animationDuration' = floor _ldAnimationDuration
    newStart = if (_linkForm <$> _ldLink) == (_linkForm <$> oldLink)
        then _lsStart oldState
        else ts - animationDuration' + delta
