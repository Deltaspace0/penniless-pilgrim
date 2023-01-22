{-# LANGUAGE FlexibleContexts #-}

module Widgets.GameControlNode
    ( module Widgets.GameControlNode.NodeData
    , module Widgets.GameControlNode.NodeMessage
    , module Widgets.GameControlNode.NodeVisual
    , gameControlNode
    ) where

import Control.Lens
import Data.Default
import Data.Typeable
import Monomer
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

import Widgets.ButtonColors
import Widgets.GameControlNode.NodeData
import Widgets.GameControlNode.NodeMessage
import Widgets.GameControlNode.NodeRenderer
import Widgets.GameControlNode.NodeState
import Widgets.GameControlNode.NodeVisual

gameControlNode
    :: (ButtonColors c)
    => NodeData s c
    -> WidgetNode s e
gameControlNode nodeData = node where
    node = defaultWidgetNode widgetType widget
    widgetType = gameControlNodeWidgetType nodeData
    widget = makeGameControlNode nodeData def

makeGameControlNode
    :: (ButtonColors c)
    => NodeData s c
    -> NodeState
    -> Widget s e
makeGameControlNode nodeData state = widget where
    widget = createSingle state def
        { singleGetBaseStyle = getBaseStyle
        , singleGetCurrentStyle = getCurrentStyle
        , singleInit = init'
        , singleMerge = merge
        , singleHandleEvent = handleEvent
        , singleHandleMessage = handleMessage
        , singleRender = render
        }

    getBaseStyle _ _ = baseStyleFromNodeData nodeData

    getCurrentStyle wenv node = style where
        vp = node ^. L.info . L.viewport
        style = currentStyle_ c wenv node
        c = def & L.isHovered .~ isNodeHoveredEllipse_ vp

    init' _ node = resultNode resNode where
        resNode = node & L.widget .~ w
        w = makeGameControlNode nodeData state'
        state' = initState nodeData

    merge wenv newNode _ oldState = result where
        result = resultReqs resNode reqs
        resNode = newNode & L.widget .~ w
        w = makeGameControlNode nodeData newState
        (newState, reqs) = mergeState oldState ts nodeData newNode
        ts = wenv ^. L.timestamp

    handleEvent _ node _ event = case event of
        Enter _ -> makeResult reqs where
            reqs = widgetDataSet nextTaxField nextTax
        Leave _ -> makeResult reqs where
            reqs = widgetDataSet nextTaxField Nothing
        Click p _ _ | valid -> makeResult reqs where
            valid = isPointInNodeVp node p && clickable
            reqs =
                [ SendMessage gcId position
                , ResetCursorIcon id'
                ]
        _ -> Nothing
        where
            makeResult = Just . resultReqs node
            gcId = _ndGameControlId nodeData
            id' = node ^. L.info . L.widgetId
            nextTaxField = _ndNextTaxLens nodeData
            nextTax = _ndNextTax nodeData
            clickable = _ndClickable nodeData
            position = _ndPosition nodeData

    handleMessage wenv node _ message = do
        let ts = wenv ^. L.timestamp
            f = changeState state ts nodeData node
        (state', reqs) <- f <$> cast message
        let w = makeGameControlNode nodeData state'
            newNode = node & L.widget .~ w
        return $ resultReqs newNode reqs

    render wenv node renderer = runRenderer $ NodeRenderer
        { _nrEnv = wenv
        , _nrNode = node
        , _nrRenderer = renderer
        , _nrNodeData = nodeData
        , _nrNodeState = state
        }
