{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControl
    ( GameControlData(..)
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
import Model hiding (Node, Link)

data GameControlData s = GameControlData
    { _gcdGame :: ALens' s Game
    , _gcdNextTax :: ALens' s (Maybe Double)
    , _gcdColors :: Colors
    , _gcdAnimationDuration :: Double
    , _gcdLinkToNodeRatio :: Double
    , _gcdNodeToWidthRatio :: Double
    , _gcdWidth :: Double
    , _gcdHeight :: Double
    }

newtype GameControlState = GameControlState
    { _gcsGrid :: Grid Node Link
    }

gridFromGame :: Game -> Colors -> Grid Node Link
gridFromGame game colors = gridMap nt hlt vlt $ _grid game where
    nt  = nodeTransform colors
    hlt = hlinkTransform colors
    vlt = vlinkTransform colors

gameControl :: GameControlData s -> WidgetNode s e
gameControl gcData = gameControlNode where
    gameControlNode = defaultWidgetNode "gameControl" widget
    widget = makeGameControl gcData state
    state = GameControlState $ makeGrid 2 2

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
        }

    init wenv node = resultReqs resNode reqs where
        resNode = node
            & L.widget .~ w
            & L.children .~
                   fmap fn nodeSequence
                >< fmap (fh . snd) hlinkSequence
                >< fmap (fl . snd) vlinkSequence
        reqs = [ResizeWidgets widgetId]
        w = makeGameControl gcData $ GameControlState grid
        game = widgetDataGet (wenv ^. L.model) gameField
        grid = gridFromGame game colors
        nodeSequence = getNodeSequence grid
        hlinkSequence = getHlinkSequence grid
        vlinkSequence = getVlinkSequence grid
        fn (p, nodeStack) = gameControlNode $ NodeData
            { _ndNodeStack = nodeStack
            , _ndColor = _nodeDefault colors
            , _ndHoverColor = _nodeHover colors
            , _ndActiveColor = _nodeActive colors
            , _ndHighlightColor = _nodeHighlight colors
            , _ndGameControlId = widgetId
            , _ndDirection = directionFromGame p game
            , _ndNextTax = taxFromGame p game
            , _ndNextTaxField = nextTaxField
            , _ndAnimationDuration = animationDuration
            }
        fh = gameControlHlink . linkData
        fl = gameControlVlink . linkData
        linkData link = LinkData
            { _ldLink = link
            , _ldColor = _linkDefault colors
            , _ldAnimationDuration = animationDuration
            , _ldNodeToWidthRatio = nodeToWidthRatio
            }
        widgetId = node ^. L.info . L.widgetId

    merge wenv newNode _ _ = init wenv newNode

    handleEvent wenv node target evt = case evt of
        KeyAction _ code KeyPressed
            | isKeyNorth code -> handleDirection wenv node North
            | isKeySouth code -> handleDirection wenv node South
            | isKeyWest code -> handleDirection wenv node West
            | isKeyEast code -> handleDirection wenv node East
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
        result = resultReqs newNode reqs
        newNode = node & L.widget .~ w
        reqs = concat $
            [ [RenderOnce]
            , widgetDataSet gameField game'
            , widgetDataSet nextTaxField Nothing
            ]
        w = makeGameControl gcData state'
        game' = movePilgrim direction game
        game = widgetDataGet (wenv ^. L.model) gameField
        state' = GameControlState $ gridFromGame game' colors

    getNodeArea vp (i, j) = Rect x y d d where
        x = (vx vp) + linkSize*(fromIntegral i)
        y = (vy vp) + linkSize*(fromIntegral j)
        d = nodeSize*2

    getLinkArea vp (i, j) = Rect x y linkSize nodeSize where
        x = (vx vp) + linkSize*(fromIntegral i) + nodeSize
        y = (vy vp) + linkSize*(fromIntegral j) + nodeSize

    gameField = WidgetLens $ _gcdGame gcData
    nextTaxField = WidgetLens $ _gcdNextTax gcData
    grid = _gcsGrid state
    colors = _gcdColors gcData
    animationDuration = _gcdAnimationDuration gcData
    linkToNodeRatio = _gcdLinkToNodeRatio gcData
    nodeToWidthRatio = _gcdNodeToWidthRatio gcData
    width = _gcdWidth gcData
    height = _gcdHeight gcData
    (cols, rows) = getBounds grid
    factorW = (fromIntegral cols)+2/linkToNodeRatio
    factorH = (fromIntegral rows)+2/linkToNodeRatio
    linkSize = min (width/factorW) (height/factorH)
    nodeSize = linkSize/linkToNodeRatio
    vx vp = (width-linkSize*factorW)/2 + vp ^. L.x
    vy vp = (height-linkSize*factorH)/2 + vp ^. L.y