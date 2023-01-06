module Widgets.GameControlNode.NodeMessage
    ( NodeMessage(..)
    ) where

data NodeMessage
    = NodeStartShake
    | NodeStopShake
    | NodeStopAnimation
    deriving (Eq, Show)
