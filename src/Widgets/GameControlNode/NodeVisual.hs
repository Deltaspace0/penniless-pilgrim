module Widgets.GameControlNode.NodeVisual
    ( NodeVisual(..)
    , nodeTransform
    ) where

import Monomer

import Model.Game
import Widgets.GameControlNode.NodeColorConfig

data NodeVisual = NodeVisual
    { _nodeColorHighlight :: Color
    , _nodeColorDefault :: Color
    , _nodeColorHover :: Color
    , _nodeColorActive :: Color
    } deriving (Eq, Show)

nodeTransform :: NodeColorConfig -> [GameNode] -> [NodeVisual]
nodeTransform config = map getVisual where
    getVisual node = f $ case node of
        NodePilgrim -> _nccPilgrim config
        NodePath -> _nccPath config
        NodeGoal -> _nccGoal config
    f colors = NodeVisual
        { _nodeColorHighlight = _nodeHighlight colors
        , _nodeColorDefault = _nodeDefault colors
        , _nodeColorHover = _nodeHover colors
        , _nodeColorActive = _nodeActive colors
        }