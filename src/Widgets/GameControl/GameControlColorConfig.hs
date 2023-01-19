module Widgets.GameControl.GameControlColorConfig
    ( module Widgets.GameControlLink.LinkColorConfig
    , module Widgets.GameControlNode.NodeColorConfig
    , GameControlColorConfig
    , getLinkColorConfig
    , getNodeColorConfig
    ) where

import Widgets.GameControlLink.LinkColorConfig
import Widgets.GameControlNode.NodeColorConfig

class GameControlColorConfig a where
    getLinkColorConfig :: a -> LinkColorConfig
    getNodeColorConfig :: a -> NodeColorConfig
