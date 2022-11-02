{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControl
    ( GameControlCfg(..)
    , gameControl
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Sequence ((><))
import Data.Typeable
import Monomer
import Monomer.Widgets.Container
import qualified Monomer.Lens as L

import Widgets.GameControl.Link
import Widgets.GameControl.Node
import Model.Game hiding (Node, Link)
import Model.Grid
import Model.Parameters.Colors

data GameControlCfg = GameControlCfg
    { _colors :: Colors
    , _linkToNodeRatio  :: Double
    , _nodeToWidthRatio :: Double
    , _gcWidth  :: Double
    , _gcHeight :: Double
    } deriving (Eq, Show)

newtype GameControlState = GameControlState
    { _gcsGrid :: Grid Node Link
    }

gridFromGame :: Game -> GameControlCfg -> Grid Node Link
gridFromGame game config = gridMap nt hlt vlt $ _grid game where
    nt  = nodeTransform $ _colors config
    hlt = hlinkTransform $ _colors config
    vlt = vlinkTransform $ _colors config

gameControl :: ALens' s Game -> GameControlCfg -> WidgetNode s e
gameControl field config = gameControlNode where
    gameControlNode = defaultWidgetNode "gameControl" widget
    widget = makeGameControl (WidgetLens field) config state
    state = GameControlState $ makeGrid 2 2

makeGameControl
    :: WidgetData s Game
    -> GameControlCfg
    -> GameControlState
    -> Widget s e
makeGameControl field config state = widget where
    widget = createContainer state def
        { containerInit = init
        , containerMerge = merge
        , containerHandleEvent = handleEvent
        , containerHandleMessage = handleMessage
        , containerGetSizeReq = getSizeReq
        , containerResize = resize
        }

    init wenv node = resultNode resNode where
        resNode = node
            & L.widget .~ w
            & L.children .~
                   fmap fn nodeSequence
                >< fmap (fh . snd) hlinkSequence
                >< fmap (fl . snd) vlinkSequence
        w = makeGameControl field config $ GameControlState grid
        game = widgetDataGet (wenv ^. L.model) field
        grid = gridFromGame game config
        nodeSequence = getNodeSequence grid
        hlinkSequence = getHlinkSequence grid
        vlinkSequence = getVlinkSequence grid
        fn (p, nodeStack) = gameControlNode nodeStack $ NodeCfg
            { _ncColor = _nodeDefault colors
            , _ncHoverColor = _nodeHover colors
            , _ncActiveColor = _nodeActive colors
            , _ncHighlightColor = _nodeHighlight colors
            , _ncGameControlId = node ^. L.info . L.widgetId
            , _ncDirection = directionFromGame p game
            }
        fh = flip gameControlHlink linkConfig
        fl = flip gameControlVlink linkConfig
        linkConfig = LinkCfg
            { _lcColor = _linkDefault colors
            , _lcNodeToWidthRatio = nodeToWidthRatio
            }

    merge wenv newNode _ _ = init wenv newNode

    handleEvent wenv node target evt = case evt of
        KeyAction _ code KeyPressed
            | isKeyNorth code -> handleDirection wenv node North
            | isKeySouth code -> handleDirection wenv node South
            | isKeyWest  code -> handleDirection wenv node West
            | isKeyEast  code -> handleDirection wenv node East
            where
                isKeyNorth code = isKeyUp    code || isKeyW code
                isKeySouth code = isKeyDown  code || isKeyS code
                isKeyWest  code = isKeyLeft  code || isKeyA code
                isKeyEast  code = isKeyRight code || isKeyD code
        ButtonAction _ _ BtnPressed _ -> Just result where
            result = resultReqs node [SetFocus widgetId]
        _ -> Nothing
        where
            widgetId = node ^. L.info . L.widgetId

    handleMessage wenv node target message = result where
        result = cast message >>= handleDirection wenv node

    getSizeReq wenv node _ = (fixedSize width, fixedSize height)

    resize wenv node vp children = resized where
        resized = (resultNode node, assignedAreas)
        assignedAreas =
               fmap (getNodeArea vp . fst) nodeSequence
            >< fmap (getLinkArea vp . fst) hlinkSequence
            >< fmap (getLinkArea vp . fst) vlinkSequence
        nodeSequence = getNodeSequence grid
        hlinkSequence = getHlinkSequence grid
        vlinkSequence = getVlinkSequence grid

    handleDirection wenv node direction = Just result where
        result = resultReqs newNode $ RenderOnce:reqs
        newNode = node & L.widget .~ w
        reqs = widgetDataSet field game'
        w = makeGameControl field config state'
        game' = movePilgrim direction game
        game = widgetDataGet (wenv ^. L.model) field
        state' = GameControlState $ gridFromGame game' config

    getNodeArea vp (i, j) = Rect x y d d where
        x = (vx vp) + linkSize*(fromIntegral i)
        y = (vy vp) + linkSize*(fromIntegral j)
        d = nodeSize*2

    getLinkArea vp (i, j) = Rect x y linkSize nodeSize where
        x = (vx vp) + linkSize*(fromIntegral i) + nodeSize
        y = (vy vp) + linkSize*(fromIntegral j) + nodeSize

    grid = _gcsGrid state
    colors = _colors config
    linkToNodeRatio = _linkToNodeRatio config
    nodeToWidthRatio = _nodeToWidthRatio config
    width = _gcWidth config
    height = _gcHeight config
    (cols, rows) = getBounds grid
    factorW = (fromIntegral cols)+2/linkToNodeRatio
    factorH = (fromIntegral rows)+2/linkToNodeRatio
    linkSize = min (width/factorW) (height/factorH)
    nodeSize = linkSize/linkToNodeRatio
    vx vp = (width-linkSize*factorW)/2  + vp ^. L.x
    vy vp = (height-linkSize*factorH)/2 + vp ^. L.y