module Widgets.GameControl.NodeVisual
    ( NodeVisual(..)
    ) where

import Monomer

data NodeVisual = NodeVisual
    { _nodeColorHighlight :: Color
    , _nodeColorDefault :: Color
    , _nodeColorHover :: Color
    , _nodeColorActive :: Color
    } deriving (Eq, Show)
