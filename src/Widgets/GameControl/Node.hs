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
import Control.Monad
import Data.Default
import Data.Maybe
import Monomer
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

import Model.Parameters.Colors
import qualified Model.Game as G

data Node = Node
    { _nodeColor :: Color
    , _nodeHoverColor :: Color
    , _nodeActiveColor :: Color
    } deriving (Eq, Show)

data NodeCfg = NodeCfg
    { _ncColor :: Color
    , _ncHoverColor :: Color
    , _ncActiveColor :: Color
    , _ncGameControlId :: WidgetId
    , _ncDirection :: Maybe G.Direction
    } deriving (Eq, Show)

makeFields 'Node

nodeTransform :: Colors -> [G.Node] -> [Node]
nodeTransform colors = map $ \node -> case node of
    G.NodePilgrim -> Node
        { _nodeColor = _nodePilgrimDefault colors
        , _nodeHoverColor = _nodePilgrimHover colors
        , _nodeActiveColor = _nodePilgrimActive colors
        }
    G.NodePath -> Node
        { _nodeColor = _nodePathDefault colors
        , _nodeHoverColor = _nodePathHover colors
        , _nodeActiveColor = _nodePathActive colors
        }
    G.NodeGoal -> Node
        { _nodeColor = _nodeGoalDefault colors
        , _nodeHoverColor = _nodeGoalHover colors
        , _nodeActiveColor = _nodeGoalActive colors
        }

gameControlNode :: [Node] -> NodeCfg -> WidgetNode s e
gameControlNode nodeStack config = gameControlNodeNode where
    gameControlNodeNode = defaultWidgetNode "gameControlNode" widget
    widget = makeGameControlNode nodeStack config

makeGameControlNode :: [Node] -> NodeCfg -> Widget s e
makeGameControlNode nodeStack config = widget where
    widget = createSingle () def
        { singleGetBaseStyle = getBaseStyle
        , singleGetCurrentStyle = getCurrentStyle
        , singleHandleEvent = handleEvent
        , singleRender = render
        }

    getBaseStyle _ _ = if null direction
        then Just basicStyle
        else Just $ basicStyle
            { _styleHover = Just $ def
                { _sstFgColor = Just $ if null nodeStack
                    then _ncHoverColor config
                    else nodeHead ^. hoverColor
                , _sstCursorIcon = Just CursorHand
                }
            , _styleActive = Just $ def
                { _sstFgColor = Just $ if null nodeStack
                    then _ncActiveColor config
                    else nodeHead ^. activeColor
                , _sstCursorIcon = Just CursorHand
                }
            }

    getCurrentStyle wenv node = style where
        vp = node ^. L.info . L.viewport
        style = currentStyle_ c wenv node
        c = def & L.isHovered .~ isNodeHoveredEllipse_ vp

    handleEvent wenv node target evt = case evt of
        Click p _ _
            | isPointInNodeVp node p && isDirection -> Just result
        _ -> Nothing
        where
            isDirection = not $ null direction
            result = resultReqs node reqs
            reqs = [SendMessage gcId direction', ResetCursorIcon id]
            gcId = _ncGameControlId config
            direction' = fromJust direction
            id = node ^. L.info . L.widgetId

    render wenv node renderer = do
        let style = currentStyle wenv node
            vp = getContentArea node style
        drawEllipse renderer vp $ _sstFgColor style
        when (not $ null direction) $ do
            drawEllipseBorder renderer vp (Just white) 2

    nodeHead = head nodeStack
    direction = _ncDirection config
    basicStyle = def
        { _styleBasic = Just $ def
            { _sstFgColor = Just $ if null nodeStack
                then _ncColor config
                else nodeHead ^. color
            }
        }