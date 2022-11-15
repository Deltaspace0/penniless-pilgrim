{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControl
    ( GameControlData(..)
    , gameControl
    ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Sequence ((><))
import Data.Typeable
import Monomer
import Monomer.Widgets.Container
import qualified Data.Sequence as Seq
import qualified Monomer.Lens as L

import Widgets.GameControl.Link
import Widgets.GameControl.Node
import Model hiding (Node, Link)

data GameControlData s = GameControlData
    { _gcdGameLens :: ALens' s Game
    , _gcdNextTaxLens :: ALens' s (Maybe Double)
    , _gcdColors :: Colors
    , _gcdAnimationDuration :: Double
    , _gcdLinkToNodeRatio :: Double
    , _gcdNodeToWidthRatio :: Double
    , _gcdWidth :: Double
    , _gcdHeight :: Double
    }

data GameControlState = GameControlState
    { _gcsGrid :: Grid Node Link
    , _gcsPreviousBounds :: (Double, Double)
    , _gcsRunning :: Bool
    , _gcsStart :: Millisecond
    } deriving (Eq, Show)

instance Default GameControlState where
    def = GameControlState
        { _gcsGrid = makeGrid 2 2
        , _gcsPreviousBounds = (0, 0)
        , _gcsRunning = False
        , _gcsStart = 0
        }

gridFromGame :: Game -> Colors -> Grid Node Link
gridFromGame game colors = gridMap nt hlt vlt $ _grid game where
    nt  = nodeTransform colors
    hlt = hlinkTransform colors
    vlt = vlinkTransform colors

gameControl :: GameControlData s -> WidgetNode s e
gameControl gcData = gameControlNode where
    gameControlNode = defaultWidgetNode "gameControl" widget
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
        resNode = (makeChildren wenv node) & L.widget .~ w
        w = makeGameControl gcData $ state
            { _gcsGrid = grid
            }
        game = widgetDataGet (wenv ^. L.model) gameLens
        grid = gridFromGame game colors

    merge wenv newNode _ oldState = resultReqs resNode reqs where
        resNode = (makeChildren wenv newNode) & L.widget .~ w
        w = makeGameControl gcData state'
        state' = if newBounds == oldBounds
            then oldState
            else GameControlState
                { _gcsGrid = grid'
                , _gcsPreviousBounds = previousBounds
                , _gcsRunning = animationDuration' > 0
                , _gcsStart = ts
                }
        reqs = if newBounds == oldBounds
            then [ResizeWidgets widgetId]
            else [RenderEvery widgetId period (Just steps)]
        grid' = gridFromGame game colors
        grid = _gcsGrid oldState
        game = widgetDataGet (wenv ^. L.model) gameLens
        newBounds = getBounds grid'
        oldBounds = getBounds grid
        cols = fst $ _gcsPreviousBounds oldState
        rows = snd $ _gcsPreviousBounds oldState
        cols' = fromIntegral $ fst oldBounds
        rows' = fromIntegral $ snd oldBounds
        cols'' = cols+(cols'-cols)*progress
        rows'' = rows+(rows'-rows)*progress
        oldStart = _gcsStart oldState
        delta = fromIntegral (ts-oldStart) :: Double
        progress = delta/animationDuration
        previousBounds = if delta < animationDuration
            then (cols'', rows'')
            else (cols', rows')
        ts = wenv ^. L.timestamp
        widgetId = newNode ^. L.info . L.widgetId
        period = 10
        steps = fromIntegral $ animationDuration' `div` period
        animationDuration' = floor animationDuration

    handleEvent wenv node _ event = case event of
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

    handleMessage wenv node _ message = result where
        result = cast message >>= handlePosition wenv node

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

    render wenv node renderer = do
        let style = currentStyle wenv node
            vp = getContentArea node style
            ts = wenv ^. L.timestamp
            (cols', rows') = _gcsPreviousBounds state
            running = _gcsRunning state
            start = _gcsStart state
            delta = fromIntegral $ ts-start
            progress = max 0 $ min 1 $ delta/animationDuration
        saveContext renderer
        when (running && progress < 1) $ do
            let factorW' = cols'+2/linkToNodeRatio
                factorH' = rows'+2/linkToNodeRatio
                linkSize' = min (width/factorW') (height/factorH')
                x = linkSize'*factorW/width
                y = linkSize'*factorH/height
                s = (max x y)+progress*(1-(max x y))
                vx = (width-linkSize*factorW)/2
                vy = (height-linkSize*factorH)/2
                vx' = (width-linkSize'*factorW')/2
                vy' = (height-linkSize'*factorH')/2
                tx = (vx'-vx)*(1-progress)+(1-s)*(vx+(vp ^. L.x))
                ty = (vy'-vy)*(1-progress)+(1-s)*(vy+(vp ^. L.y))
            intersectScissor renderer vp
            setTranslation renderer $ Point tx ty
            setScale renderer $ Point s s

    renderAfter _ _ = restoreContext

    makeChildren wenv node = resNode where
        resNode = node & L.children .~ fmap fn nodeSequence
            >< fmap fh hlinkSequence
            >< fmap fv vlinkSequence
        game = widgetDataGet (wenv ^. L.model) gameLens
        grid = gridFromGame game colors
        nodeSequence = getNodeSequence grid
        hlinkSequence = getHlinkSequence grid
        vlinkSequence = getVlinkSequence grid
        fn (p, nodeStack) = gameControlNode $ NodeData
            { _ndNodeStack = nodeStack
            , _ndNullColor = _nodeDefault colors
            , _ndNullHoverColor = _nodeHover colors
            , _ndNullActiveColor = _nodeActive colors
            , _ndHighlightColor = _nodeHighlight colors
            , _ndGameControlId = widgetId
            , _ndPosition = p
            , _ndClickable = not $ null tax
            , _ndNextTax = tax
            , _ndNextTaxLens = nextTaxLens
            , _ndAnimationDuration = animationDuration
            } where tax = taxFromGame p game
        fh = gameControlHlink . linkData
        fv = gameControlVlink . linkData
        linkData (p, link) = LinkData
            { _ldLink = link
            , _ldNullColor = _linkDefault colors
            , _ldPosition = p
            , _ldAnimationDuration = animationDuration
            , _ldNodeToWidthRatio = nodeToWidthRatio
            }
        widgetId = node ^. L.info . L.widgetId

    handleDirection wenv node direction = result where
        result = handleGameChange wenv node f
        f = movePilgrim direction

    handlePosition wenv node p = result where
        result = handleGameChange wenv node f
        f = jumpPilgrim p

    handleGameChange wenv node f = result where
        result = Just $ resultReqs node reqs
        nextGame = f game
        nextGame' = fromJust nextGame
        game = widgetDataGet (wenv ^. L.model) gameLens
        reqs = if null nextGame
            then [SendMessage shakeNodeId NodeStartShake]
            else concat
                [ widgetDataSet gameLens nextGame'
                , widgetDataSet nextTaxLens Nothing
                ]
        shakeNodeId = shakeNode ^. L.info ^. L.widgetId
        shakeNode = Seq.index (node ^. L.children) $ x*(rows+1)+y
        (x, y) = _position $ _pilgrim game

    getNodeArea vp (i, j) = Rect x y d d where
        x = (vx vp) + linkSize*(fromIntegral i)
        y = (vy vp) + linkSize*(fromIntegral j)
        d = nodeSize*2

    getLinkArea vp (i, j) = Rect x y linkSize nodeSize where
        x = (vx vp) + linkSize*(fromIntegral i) + nodeSize
        y = (vy vp) + linkSize*(fromIntegral j) + nodeSize

    gameLens = WidgetLens $ _gcdGameLens gcData
    nextTaxLens = WidgetLens $ _gcdNextTaxLens gcData
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