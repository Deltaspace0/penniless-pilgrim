{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Widgets.GameControl.Node
    ( Node(..)
    , NodeCfg(..)
    , nodeTransform
    , gameControlNode
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Monomer
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

import Model.Game hiding (Node)
import Model.Parameters.Colors
import qualified Model.Game as G

data Node = Node
    { _nodeColor       :: Color
    , _nodeHoverColor  :: Color
    , _nodeActiveColor :: Color
    } deriving (Eq, Show)

data NodeCfg = NodeCfg
    { _defaultColor       :: Color
    , _defaultHoverColor  :: Color
    , _defaultActiveColor :: Color
    } deriving (Eq, Show)

makeFields 'Node

nodeTransform :: Colors -> [G.Node] -> [Node]
nodeTransform colors = map $ \node -> case node of
    NodePilgrim -> Node
        { _nodeColor       = _nodePilgrimDefault colors
        , _nodeHoverColor  = _nodePilgrimHover colors
        , _nodeActiveColor = _nodePilgrimActive colors
        }
    NodePath -> Node
        { _nodeColor       = _nodePathDefault colors
        , _nodeHoverColor  = _nodePathHover colors
        , _nodeActiveColor = _nodePathActive colors
        }
    NodeGoal -> Node
        { _nodeColor       = _nodeGoalDefault colors
        , _nodeHoverColor  = _nodeGoalHover colors
        , _nodeActiveColor = _nodeGoalActive colors
        }

gameControlNode :: [Node] -> NodeCfg -> WidgetNode s e
gameControlNode nodeStack config = gameControlNodeNode where
    gameControlNodeNode = defaultWidgetNode "gameControlNode" widget
    widget = makeGameControlNode nodeStack config

makeGameControlNode :: [Node] -> NodeCfg -> Widget s e
makeGameControlNode nodeStack config = widget where
    widget = createSingle () def
        { singleGetBaseStyle    = getBaseStyle
        , singleGetCurrentStyle = getCurrentStyle
        , singleRender          = render
        }

    getBaseStyle _ _ = Just $ def
        { _styleBasic = Just $ def
            { _sstFgColor = Just $ if null nodeStack
                then _defaultColor config
                else nodeHead ^. color
            }
        , _styleHover = Just $ def
            { _sstFgColor = Just $ if null nodeStack
                then _defaultHoverColor config
                else nodeHead ^. hoverColor
            , _sstCursorIcon = Just CursorHand
            }
        , _styleActive = Just $ def
            { _sstFgColor = Just $ if null nodeStack
                then _defaultActiveColor config
                else nodeHead ^. activeColor
            , _sstCursorIcon = Just CursorHand
            }
        }

    getCurrentStyle wenv node = style where
        vp = node ^. L.info . L.viewport
        style = currentStyle_ c wenv node
        c = def & L.isHovered .~ isNodeHoveredEllipse_ vp

    render wenv node renderer = do
        let style = currentStyle wenv node
            vp = getContentArea node style
        drawEllipse renderer vp $ _sstFgColor style

    nodeHead = head nodeStack