{-# LANGUAGE RecordWildCards #-}

module Widgets.GameControlNode.NodeState
    ( module Widgets.GameControlNode.ShakeState
    , module Widgets.GameControlNode.VisualState
    , NodeState(..)
    , initState
    , changeState
    , mergeState
    ) where

import Control.Lens
import Data.Default
import Monomer

import Common.Util
import Widgets.GameControlNode.NodeData
import Widgets.GameControlNode.NodeMessage
import Widgets.GameControlNode.ShakeState
import Widgets.GameControlNode.VisualState

data NodeState = NodeState
    { _nsVisualStates :: [VisualState]
    , _nsShakeState :: ShakeState
    } deriving (Eq, Show)

instance Default NodeState where
    def = NodeState def def

initState :: NodeData s -> NodeState
initState nodeData = def
    { _nsVisualStates =
        [ VisualState
            { _vsVisual = _ndVisualStack nodeData ^? ix 0
            , _vsStart = -1000000
            }
        ]
    }

changeState
    :: NodeState
    -> Millisecond
    -> NodeData s
    -> WidgetNode s e
    -> NodeMessage
    -> (NodeState, [WidgetRequest s e])
changeState state ts nodeData node message = (state', reqs) where
    state' = case message of
        NodeStartShake -> state
            { _nsShakeState = ShakeState
                { _ssRunning = True
                , _ssStart = ts
                }
            }
        NodeStopShake -> state
            { _nsShakeState = def
            }
        NodeStopAnimation -> def
    reqs = if message == NodeStartShake
        then [requestRenderEvery node animationDuration]
        else []
    animationDuration = _ndAnimationDuration nodeData

mergeState
    :: NodeState
    -> Millisecond
    -> NodeData s
    -> WidgetNode s e
    -> (NodeState, [WidgetRequest s e])
mergeState oldState ts NodeData{..} newNode = (newState, reqs) where
    newState = if newVisual == oldVisual
        then oldState
        else oldState
            { _nsVisualStates = newVisualStates
            }
    reqs = if newVisual == oldVisual
        then []
        else [requestRenderEvery newNode _ndAnimationDuration]
    newVisual = _ndVisualStack ^? ix 0
    oldVisual = _vsVisual =<< oldVisualStates ^? ix 0
    oldVisualStates = _nsVisualStates oldState
    newVisualStates = if null oldVisualStates
        then [visualState, def]
        else visualState:(oldRunningStates ++ (take 1 notRunning))
    visualState = VisualState
        { _vsVisual = newVisual
        , _vsStart = ts
        }
    (oldRunningStates, notRunning) = span f oldVisualStates
    f = isRunningVisualState _ndAnimationDuration ts
