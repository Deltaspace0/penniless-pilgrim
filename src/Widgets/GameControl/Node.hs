{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Widgets.GameControl.Node
    ( Node(..)
    , NodeData(..)
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

data NodeData s = NodeData
    { _ndNodeStack :: [Node]
    , _ndColor :: Color
    , _ndHoverColor :: Color
    , _ndActiveColor :: Color
    , _ndHighlightColor :: Color
    , _ndGameControlId :: WidgetId
    , _ndDirection :: Maybe G.Direction
    , _ndNextTax :: Maybe Double
    , _ndNextTaxField :: WidgetData s (Maybe Double)
    , _ndAnimationDuration :: Double
    }

data NodeState = NodeState
    { _nsAnimationStack :: [(Millisecond, Maybe Node)]
    , _nsRunning :: Bool
    } deriving (Eq, Show)

instance Default NodeState where
    def = NodeState
        { _nsAnimationStack = []
        , _nsRunning = False
        }

gameControlNode :: NodeData s -> WidgetNode s e
gameControlNode nodeData = node where
    node = defaultWidgetNode "gameControlNode" widget
    widget = makeGameControlNode nodeData def

makeGameControlNode :: NodeData s -> NodeState -> Widget s e
makeGameControlNode nodeData state = widget where
    widget = createSingle state def
        { singleGetBaseStyle = getBaseStyle
        , singleGetCurrentStyle = getCurrentStyle
        , singleInit = init
        , singleMerge = merge
        , singleHandleEvent = handleEvent
        , singleRender = render
        }

    getBaseStyle _ _ = if null direction
        then Just basicStyle
        else Just $ basicStyle
            { _styleHover = Just $ def
                { _sstFgColor = Just $ if null nodeStack
                    then _ndHoverColor nodeData
                    else nodeHead ^. hoverColor
                , _sstCursorIcon = Just CursorHand
                }
            , _styleActive = Just $ def
                { _sstFgColor = Just $ if null nodeStack
                    then _ndActiveColor nodeData
                    else nodeHead ^. activeColor
                , _sstCursorIcon = Just CursorHand
                }
            }

    getCurrentStyle wenv node = style where
        vp = node ^. L.info . L.viewport
        style = currentStyle_ c wenv node
        c = def & L.isHovered .~ isNodeHoveredEllipse_ vp

    init _ node = resultNode resNode where
        resNode = node & L.widget .~ w
        w = makeGameControlNode nodeData state'
        state' = state
            { _nsAnimationStack = [(0, nodeHead')]
            }
        nodeHead' = if null nodeStack
            then Nothing
            else Just nodeHead

    merge wenv newNode _ oldState = result where
        result = resultReqs resNode reqs
        resNode = newNode & L.widget .~ w
        w = makeGameControlNode nodeData state'
        state' = if newNodeHead == oldNodeHead
            then oldState
            else NodeState
                { _nsAnimationStack = newAnimationStack
                , _nsRunning = animationDuration' > 0
                }
        reqs = if newNodeHead == oldNodeHead
            then []
            else [RenderEvery widgetId period (Just steps)]
        newNodeHead = if null nodeStack
            then Nothing
            else Just nodeHead
        oldNodeHead = if null oldAnimationStack
            then Nothing
            else snd oldAnimationHead
        oldAnimationStack = _nsAnimationStack oldState
        newAnimationStack = if null oldAnimationStack
            then [(ts, newNodeHead), (0, Nothing)]
            else (ts, newNodeHead):filteredAnimationStack
        oldAnimationHead = head oldAnimationStack
        filteredAnimationStack = oldAnimationHead:filteredTail
        filteredTail = filter f $ tail oldAnimationStack
        f (ts', _) = ts-ts' < animationDuration'
        ts = wenv ^. L.timestamp
        widgetId = newNode ^. L.info . L.widgetId
        period = 10
        steps = fromIntegral $ animationDuration' `div` period
        animationDuration' = floor animationDuration

    handleEvent wenv node target evt = case evt of
        Enter _ -> Just result where
            result = resultReqs node reqs
            reqs = widgetDataSet nextTaxField nextTax
        Leave _ -> Just result where
            result = resultReqs node reqs
            reqs = widgetDataSet nextTaxField Nothing
        Click p _ _ | valid -> Just result where
            valid = isPointInNodeVp node p && isDirection
            result = resultReqs node reqs
            reqs =
                [ SendMessage gcId direction'
                , ResetCursorIcon id
                ]
            isDirection = not $ null direction
            direction' = fromJust direction
        _ -> Nothing
        where
            gcId = _ndGameControlId nodeData
            id = node ^. L.info . L.widgetId
            nextTaxField = _ndNextTaxField nodeData
            nextTax = _ndNextTax nodeData

    render wenv node renderer = do
        let ts = wenv ^. L.timestamp
            animationQueue = reverse $ _nsAnimationStack state
            running = _nsRunning state
            style = currentStyle wenv node
            vp@(Rect x y w h) = getContentArea node style
            checkD = not $ null direction
            isActive = checkD && isNodeActive wenv node
            isHovered = checkD && isNodeHoveredEllipse_ vp wenv node
            hc = Just $ _ndHighlightColor nodeData
        if running
            then forM_ animationQueue $ \(start, node') -> do
                let delta = fromIntegral $ ts-start
                    p = delta/animationDuration
                    progress = max 0 $ min 1 p
                    dx = w*(1-progress)/2
                    dy = h*(1-progress)/2
                    vp' = Rect (x+dx) (y+dy) (w-dx*2) (h-dy*2)
                    activeColor' = if null node'
                        then _ndActiveColor nodeData
                        else (fromJust node') ^. activeColor
                    hoverColor' = if null node'
                        then _ndHoverColor nodeData
                        else (fromJust node') ^. hoverColor
                    color' = if null node'
                        then _ndColor nodeData
                        else (fromJust node') ^. color
                drawEllipse renderer vp' $ Just $ if isActive
                    then activeColor'
                    else if isHovered
                        then hoverColor'
                        else color'
            else drawEllipse renderer vp $ _sstFgColor style
        when (not $ null direction) $ do
            drawEllipseBorder renderer vp hc 2

    nodeStack = _ndNodeStack nodeData
    nodeHead = head nodeStack
    direction = _ndDirection nodeData
    animationDuration = _ndAnimationDuration nodeData
    basicStyle = def
        { _styleBasic = Just $ def
            { _sstFgColor = Just $ if null nodeStack
                then _ndColor nodeData
                else nodeHead ^. color
            }
        }