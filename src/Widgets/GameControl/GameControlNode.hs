{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControl.GameControlNode
    ( module Widgets.GameControl.GameControlNode.NodeVisual
    , NodeData(..)
    , NodeMessage(..)
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

import Util
import Widgets.GameControl.GameControlConfig
import Widgets.GameControl.GameControlNode.NodeVisual

data NodeData s = NodeData
    { _ndNodeStack :: [NodeVisual]
    , _ndGameControlId :: WidgetId
    , _ndPosition :: (Int, Int)
    , _ndClickable :: Bool
    , _ndNextTax :: Maybe Double
    , _ndNextTaxLens :: WidgetData s (Maybe Double)
    , _ndConfig :: GameControlConfig
    }

data NodeMessage
    = NodeStartShake
    | NodeStopShake
    | NodeStopAnimation
    deriving (Eq, Show)

data NodeState = NodeState
    { _nsAnimationStack :: [(Millisecond, Maybe NodeVisual)]
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
                    then _nodeHover colorConfig
                    else _nodeColorHover nodeHead
                , _sstCursorIcon = Just CursorHand
                }
            , _styleActive = Just $ def
                { _sstFgColor = Just $ if null nodeStack
                    then _nodeActive colorConfig
                    else _nodeColorActive nodeHead
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
            else [requestRenderEvery newNode animationDuration]
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
                then [requestRenderEvery node animationDuration]
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
            vp'@(Rect x' y' w' h') = getContentArea node style
            shakeDelta = fromIntegral $ ts-shakeStart
            p = shakeDelta/animationDuration
            shakeProgress = max 0 $ min 1 p
            sf = (1+(sin $ shakeProgress*pi*7/2))/5
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
                        then _nodeActive colorConfig
                        else _nodeColorActive $ fromJust node'
                    hoverColor' = if null node'
                        then _nodeHover colorConfig
                        else _nodeColorHover $ fromJust node'
                    color' = if null node'
                        then _nodeDefault colorConfig
                        else _nodeColorDefault $ fromJust node'
                drawEllipse renderer vp' $ Just $ if isActive
                    then activeColor'
                    else if isHovered
                        then hoverColor'
                        else color'
            else drawEllipse renderer vp $ _sstFgColor style
        let highlightColor = Just $ if null nodeStack
                then _nodeHighlight colorConfig
                else _nodeColorHighlight nodeHead
        when clickable $ do
            drawEllipseBorder renderer vp highlightColor 2

    nodeStack = _ndNodeStack nodeData
    nodeHead = head nodeStack
    position = _ndPosition nodeData
    clickable = _ndClickable nodeData
    animationDuration = _gccAnimationDuration config
    colorConfig = _nccDefault $ _gcccNode $ _gccColorConfig config
    config = _ndConfig nodeData
    basicStyle = def
        { _styleBasic = Just $ def
            { _sstFgColor = Just $ if null nodeStack
                then _nodeDefault colorConfig
                else _nodeColorDefault nodeHead
            }
        }