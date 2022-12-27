{-# LANGUAGE FlexibleContexts #-}

module Widgets.GameControlLink
    ( module Widgets.GameControlLink.LinkData
    , module Widgets.GameControlLink.LinkVisual
    , gameControlHlink
    , gameControlVlink
    ) where

import Control.Lens
import Data.Default
import Monomer
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

import Widgets.GameControlLink.LinkData
import Widgets.GameControlLink.LinkRenderer
import Widgets.GameControlLink.LinkState
import Widgets.GameControlLink.LinkVisual

gameControlHlink :: LinkData -> WidgetNode s e
gameControlHlink linkData = node where
    node = defaultWidgetNode widgetType widget
    widgetType = gameControlHlinkWidgetType linkData
    widget = makeGameControlLink True linkData def

gameControlVlink :: LinkData -> WidgetNode s e
gameControlVlink linkData = node where
    node = defaultWidgetNode widgetType widget
    widgetType = gameControlVlinkWidgetType linkData
    widget = makeGameControlLink False linkData def

makeGameControlLink :: Bool -> LinkData -> LinkState -> Widget s e
makeGameControlLink isHz linkData state = widget where
    widget = createSingle state def
        { singleInit = init
        , singleMerge = merge
        , singleRender = render
        }

    init _ node = resultNode resNode where
        resNode = node & L.widget .~ w
        w = makeGameControlLink isHz linkData state'
        state' = initState linkData

    merge wenv newNode _ oldState = result where
        result = resultReqs resNode reqs
        resNode = newNode & L.widget .~ w
        w = makeGameControlLink isHz linkData newState
        (newState, reqs) = mergeState oldState ts linkData newNode
        ts = wenv ^. L.timestamp

    render wenv node renderer = runRenderer $ LinkRenderer
        { _lrEnv = wenv
        , _lrNode = node
        , _lrRenderer = renderer
        , _lrLinkData = linkData
        , _lrLinkState = state
        , _lrIsHz = isHz
        }