module Widgets.GameControlNode.NodeVisual
    ( NodeVisual(..)
    ) where

import Monomer

import Widgets.ButtonColors

data NodeVisual = NodeVisual
    { _nodeColorHighlight :: Color
    , _nodeColorDefault :: Color
    , _nodeColorHover :: Color
    , _nodeColorActive :: Color
    } deriving (Eq, Show)

instance ButtonColors NodeVisual where
    getDefaultColor = _nodeColorDefault
    getHighlightColor = _nodeColorHighlight
    getHoverColor = _nodeColorHover
    getActiveColor = _nodeColorActive
