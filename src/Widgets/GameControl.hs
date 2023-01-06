{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControl
    ( module Widgets.GameControl.GameControlConfig
    , GameControlData(..)
    , gameControl
    ) where

import Control.Lens
import Data.Default
import Data.Typeable
import Monomer
import Monomer.Widgets.Container
import qualified Monomer.Lens as L

import Model
import Widgets.GameControl.GameControlConfig
import Widgets.GameControl.GameControlData
import Widgets.GameControl.GameControlRenderer
import Widgets.GameControl.GameControlState

gameControl :: GameControlData s -> WidgetNode s e
gameControl gcData = node where
    node = defaultWidgetNode "gameControl" widget
    widget = makeGameControl gcData def

makeGameControl
    :: GameControlData s
    -> GameControlState
    -> Widget s e
makeGameControl gcData state = widget where
    widget = createContainer state def
        { containerInit = init
        , containerMerge = merge
        , containerHandleEvent = handleEvent
        , containerHandleMessage = handleMessage
        , containerGetSizeReq = getSizeReq
        , containerResize = resize
        , containerRender = render
        , containerRenderAfter = renderAfter
        }

    init wenv node = resultNode resNode where
        resNode = (makeChildren wenv node gcData) & L.widget .~ w
        w = makeGameControl gcData state'
        state' = initState gcData wenv

    merge wenv newNode _ oldState = result where
        result = resultReqs resNode reqs
        resNode = (makeChildren wenv newNode gcData) & L.widget .~ w
        w = makeGameControl gcData state'
        (state', reqs) = mergeState oldState wenv ts gcData newNode
        ts = wenv ^. L.timestamp

    handleEvent wenv node _ event = case event of
        KeyAction _ code KeyPressed
            | isKeyNorth code -> handle North
            | isKeySouth code -> handle South
            | isKeyWest code -> handle West
            | isKeyEast code -> handle East
            where
                isKeyNorth code = isKeyUp code || isKeyW code
                isKeySouth code = isKeyDown code || isKeyS code
                isKeyWest code = isKeyLeft code || isKeyA code
                isKeyEast code = isKeyRight code || isKeyD code
        ButtonAction _ _ BtnPressed _ -> Just result where
            result = resultReqs node [SetFocus widgetId]
        _ -> Nothing
        where
            widgetId = node ^. L.info . L.widgetId
            handle = handleGameChange wenv node gcData . movePilgrim

    handleMessage wenv node _ message = result where
        result = cast message >>= handle
        handle = handleGameChange wenv node gcData . jumpPilgrim

    getSizeReq _ _ _ = (fixedSize width, fixedSize height) where
        width = _gccWidth $ _gcdConfig gcData
        height = _gccHeight $ _gcdConfig gcData

    resize wenv node vp _ = (resultNode node, assignedAreas) where
        assignedAreas = assignAreas wenv node vp gcData        

    render wenv node renderer = runRenderer $ GameControlRenderer
        { _gcrEnv = wenv
        , _gcrNode = node
        , _gcrRenderer = renderer
        , _gcrData = gcData
        , _gcrState = state
        }

    renderAfter _ _ = restoreContext
