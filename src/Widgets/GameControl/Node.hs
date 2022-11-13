{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Widgets.GameControl.Node
    ( Node(..)
    , NodeData(..)
    , NodeMessage(..)
    , nodeTransform
    , gameControlNode
    ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Typeable
import Monomer
import Monomer.Widgets.Single
import TextShow
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
    , _ndNullColor :: Color
    , _ndNullHoverColor :: Color
    , _ndNullActiveColor :: Color
    , _ndHighlightColor :: Color
    , _ndGameControlId :: WidgetId
    , _ndPosition :: (Int, Int)
    , _ndClickable :: Bool
    , _ndNextTax :: Maybe Double
    , _ndNextTaxLens :: WidgetData s (Maybe Double)
    , _ndAnimationDuration :: Double
    }

data NodeMessage
    = NodeStartShake
    | NodeStopShake
    | NodeStopAnimation
    deriving (Eq, Show)

data NodeState = NodeState
    { _nsAnimationStack :: [(Millisecond, Maybe Node)]
    , _nsRunning :: Bool
    , _nsShakeRunning :: Bool
    , _nsShakeStart :: Millisecond
    } deriving (Eq, Show)

instance Default NodeState where
    def = NodeState
        { _nsAnimationStack = []
        , _nsRunning = False
        , _nsShakeRunning = False
        , _nsShakeStart = 0
        }

gameControlNode :: NodeData s -> WidgetNode s e
gameControlNode nodeData = node where
    node = defaultWidgetNode (WidgetType widgetType) widget
    widgetType = "gameControlNode" <> showt position
    position = _ndPosition nodeData
    widget = makeGameControlNode nodeData def

makeGameControlNode :: NodeData s -> NodeState -> Widget s e
makeGameControlNode nodeData state = widget where
    widget = createSingle state def
        { singleGetBaseStyle = getBaseStyle
        , singleGetCurrentStyle = getCurrentStyle
        , singleInit = init
        , singleMerge = merge
        , singleHandleEvent = handleEvent
        , singleHandleMessage = handleMessage
        , singleRender = render
        }

    getBaseStyle _ _ = if clickable
        then Just $ basicStyle
            { _styleHover = Just $ def
                { _sstFgColor = Just $ if null nodeStack
                    then _ndNullHoverColor nodeData
                    else nodeHead ^. hoverColor
                , _sstCursorIcon = Just CursorHand
                }
            , _styleActive = Just $ def
                { _sstFgColor = Just $ if null nodeStack
                    then _ndNullActiveColor nodeData
                    else nodeHead ^. activeColor
                , _sstCursorIcon = Just CursorHand
                }
            }
        else Just basicStyle

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
            else oldState
                { _nsAnimationStack = newAnimationStack
                , _nsRunning = animationDuration' > 0
                }
        reqs = if newNodeHead == oldNodeHead
            then []
            else [makeRenderEveryRequest newNode]
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
        animationDuration' = floor animationDuration

    handleEvent wenv node _ event = case event of
        Enter _ -> Just result where
            result = resultReqs node reqs
            reqs = widgetDataSet nextTaxField nextTax
        Leave _ -> Just result where
            result = resultReqs node reqs
            reqs = widgetDataSet nextTaxField Nothing
        Click p _ _ | valid -> Just result where
            valid = isPointInNodeVp node p && clickable
            result = resultReqs node reqs
            reqs =
                [ SendMessage gcId position
                , ResetCursorIcon id
                ]
        _ -> Nothing
        where
            gcId = _ndGameControlId nodeData
            id = node ^. L.info . L.widgetId
            nextTaxField = _ndNextTaxLens nodeData
            nextTax = _ndNextTax nodeData

    handleMessage wenv node _ message = do
        action <- cast message
        let state' = case action of
                NodeStartShake -> state
                    { _nsShakeRunning = True
                    , _nsShakeStart = wenv ^. L.timestamp
                    }
                NodeStopShake -> state
                    { _nsShakeRunning = False
                    }
                NodeStopAnimation -> state
                    { _nsRunning = False
                    , _nsShakeRunning = False
                    }
            req = if action == NodeStartShake
                then [makeRenderEveryRequest node]
                else []
            w = makeGameControlNode nodeData state'
            newNode = node & L.widget .~ w
        return $ resultReqs newNode req

    render wenv node renderer = do
        let ts = wenv ^. L.timestamp
            animationQueue = reverse $ _nsAnimationStack state
            running = _nsRunning state
            shakeRunning = _nsShakeRunning state
            shakeStart = _nsShakeStart state
            style = currentStyle wenv node
            isActive = clickable && isNodeActive wenv node
            isHovered = clickable && isNodeHovered wenv node
            hc = Just $ _ndHighlightColor nodeData
            vp'@(Rect x' y' w' h') = getContentArea node style
            shakeDelta = fromIntegral $ ts-shakeStart
            p = shakeDelta/animationDuration
            shakeProgress = max 0 $ min 1 p
            sf = (1+(sin $ shakeProgress*3.14159265358979*7/2))/5
            dx = w'*sf/2
            dy = h'*sf/2
            vp@(Rect x y w h) = if shakeRunning
                then Rect (x'+dx) (y'+dy) (w'-dx*2) (h'-dy*2)
                else vp'
        if running
            then forM_ animationQueue $ \(start, node') -> do
                let delta = fromIntegral $ ts-start
                    p = delta/animationDuration
                    progress = max 0 $ min 1 p
                    dx = w*(1-progress)/2
                    dy = h*(1-progress)/2
                    vp' = Rect (x+dx) (y+dy) (w-dx*2) (h-dy*2)
                    activeColor' = if null node'
                        then _ndNullActiveColor nodeData
                        else (fromJust node') ^. activeColor
                    hoverColor' = if null node'
                        then _ndNullHoverColor nodeData
                        else (fromJust node') ^. hoverColor
                    color' = if null node'
                        then _ndNullColor nodeData
                        else (fromJust node') ^. color
                drawEllipse renderer vp' $ Just $ if isActive
                    then activeColor'
                    else if isHovered
                        then hoverColor'
                        else color'
            else drawEllipse renderer vp $ _sstFgColor style
        when clickable $ do
            drawEllipseBorder renderer vp hc 2

    makeRenderEveryRequest node = req where
        req = RenderEvery widgetId period $ Just steps
        widgetId = node ^. L.info . L.widgetId
        period = 10
        steps = fromIntegral $ animationDuration' `div` period
        animationDuration' = floor animationDuration

    nodeStack = _ndNodeStack nodeData
    nodeHead = head nodeStack
    position = _ndPosition nodeData
    clickable = _ndClickable nodeData
    animationDuration = _ndAnimationDuration nodeData
    basicStyle = def
        { _styleBasic = Just $ def
            { _sstFgColor = Just $ if null nodeStack
                then _ndNullColor nodeData
                else nodeHead ^. color
            }
        }