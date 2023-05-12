module Widgets.GameControl
    ( module Widgets.GameControl.GameControlCfg
    , GameControlData(..)
    , gameControl
    ) where

import Control.Lens
import Data.Default
import Data.Typeable
import Monomer
import Monomer.Widgets.Container
import qualified Monomer.Lens as L

import Model.Direction
import Widgets.GameControl.ControlledGame
import Widgets.GameControl.GameControlCfg
import Widgets.GameControl.GameControlData
import Widgets.GameControl.GameControlRenderer
import Widgets.GameControl.GameControlState

gameControl
    :: (ControlledGame a)
    => GameControlData s a
    -> WidgetNode s e
gameControl gcData = node where
    node = defaultWidgetNode "gameControl" widget
    widget = makeGameControl gcData def

makeGameControl
    :: (ControlledGame a)
    => GameControlData s a
    -> GameControlState
    -> Widget s e
makeGameControl gcData state = widget where
    widget = createContainer state def
        { containerInit = init'
        , containerMerge = merge
        , containerHandleEvent = handleEvent
        , containerHandleMessage = handleMessage
        , containerGetSizeReq = getSizeReq
        , containerResize = resize
        , containerRender = render
        , containerRenderAfter = renderAfter
        }

    init' wenv node = resultNode resNode where
        resNode = (makeChildren wenv node gcData) & L.widget .~ w
        w = makeGameControl gcData state'
        state' = initState gcData wenv $ node ^. L.info . L.viewport

    merge wenv newNode _ oldState = result where
        result = resultReqs resNode reqs
        resNode = (makeChildren wenv newNode gcData) & L.widget .~ w
        w = makeGameControl gcData state'
        (state', reqs) = mergeState oldState wenv gcData newNode

    handleEvent wenv node _ event = result where
        result = case event of
            KeyAction _ code KeyPressed
                | isKeyUp code || isKeyW code -> handle North
                | isKeyDown code || isKeyS code -> handle South
                | isKeyLeft code || isKeyA code -> handle West
                | isKeyRight code || isKeyD code -> handle East
            ButtonAction _ _ BtnPressed _ -> Just result' where
                result' = resultReqs node [SetFocus widgetId]
            _ -> Nothing
        widgetId = node ^. L.info . L.widgetId
        handle = handleGameChange wenv node gcData . moveToDirection

    handleMessage wenv node _ message = result where
        result = cast message >>= handle
        handle = handleGameChange wenv node gcData . moveToPosition

    getSizeReq _ _ _ = (w, h) where
        w = rangeSize width' 2000 1
        h = rangeSize height' 2000 1
        width' = getWidth $ _gcdConfig gcData
        height' = getHeight $ _gcdConfig gcData

    resize wenv node vp _ = (resultNode node', assignedAreas) where
        node' = node & L.widget .~ makeGameControl gcData state'
        state' = state
            { _gcsFixedRect = getFixedRect gcData wenv vp
            }
        assignedAreas = assignAreas wenv vp gcData        

    render wenv node renderer = runRenderer $ GameControlRenderer
        { _gcrEnv = wenv
        , _gcrNode = node
        , _gcrRenderer = renderer
        , _gcrData = gcData
        , _gcrState = state
        }

    renderAfter _ _ = restoreContext
