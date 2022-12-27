module Widgets.GameControlNode.NodeState
    ( module Widgets.GameControlNode.ShakeState
    , module Widgets.GameControlNode.VisualState
    , NodeState(..)
    , initState
    , changeState
    , mergeState
    ) where

import Data.Default
import Monomer

import Util
import Widgets.GameControlNode.NodeData
import Widgets.GameControlNode.NodeMessage
import Widgets.GameControlNode.NodeVisual
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
    { _nsVisualStates = [visualState]
    } where
        visualState = VisualState
            { _vsVisual = visual
            , _vsStart = -1000000
            }
        visual = if null visualStack
            then Nothing
            else Just $ head visualStack
        visualStack = _ndVisualStack nodeData

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
    animationDuration = getAnimationDuration nodeData

mergeState
    :: NodeState
    -> Millisecond
    -> NodeData s
    -> WidgetNode s e
    -> (NodeState, [WidgetRequest s e])
mergeState oldState ts nodeData newNode = (newState, reqs) where
    newState = if newVisual == oldVisual
        then oldState
        else oldState
            { _nsVisualStates = newVisualStates
            }
    reqs = if newVisual == oldVisual
        then []
        else [requestRenderEvery newNode animationDuration]
    newVisual = if null visualStack
        then Nothing
        else Just $ head visualStack
    visualStack = _ndVisualStack nodeData
    oldVisual = if null oldVisualStates
        then Nothing
        else _vsVisual $ head oldVisualStates
    oldVisualStates = _nsVisualStates oldState
    newVisualStates = if null oldVisualStates
        then [visualState, def]
        else visualState:(head oldVisualStates):filteredTail
    visualState = VisualState
        { _vsVisual = newVisual
        , _vsStart = ts
        }
    filteredTail = filter f $ tail oldVisualStates
    f = isRunningVisualState animationDuration ts
    animationDuration = getAnimationDuration nodeData