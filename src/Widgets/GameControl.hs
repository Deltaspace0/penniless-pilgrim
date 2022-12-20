{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControl
    ( module Widgets.GameControl.GameControlConfig
    , GameControlData(..)
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

import Model
import Util
import Widgets.GameControl.GameControlConfig
import Widgets.GameControl.GameControlLink
import Widgets.GameControl.GameControlNode

data GameControlData s = GameControlData
    { _gcdGameLens :: ALens' s Game
    , _gcdNextTaxLens :: ALens' s (Maybe Double)
    , _gcdConfig :: GameControlConfig
    }

data GameControlState = GameControlState
    { _gcsCornerRect :: Rect
    , _gcsOldCornerRect :: Rect
    , _gcsRunning :: Bool
    , _gcsStart :: Millisecond
    } deriving (Eq, Show)

instance Default GameControlState where
    def = GameControlState
        { _gcsCornerRect = Rect 0 0 0 0
        , _gcsOldCornerRect = Rect 0 0 0 0
        , _gcsRunning = False
        , _gcsStart = 0
        }

gridFromGame
    :: Game
    -> GameControlConfig
    -> Grid NodeVisual LinkVisual
gridFromGame game config = gridMap nt hlt vlt $ _grid game where
    nt  = nodeTransform nodeColorConfig
    hlt = hlinkTransform linkColorConfig
    vlt = vlinkTransform linkColorConfig
    nodeColorConfig = _gcccNode colorConfig
    linkColorConfig = _gcccLink colorConfig
    colorConfig = _gccColorConfig config

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
        resNode = (makeChildren wenv node) & L.widget .~ w
        w = makeGameControl gcData $ state
            { _gcsCornerRect = getCornerRect wenv
            , _gcsOldCornerRect = getCornerRect wenv
            }

    merge wenv newNode _ oldState = resultReqs resNode reqs where
        resNode = (makeChildren wenv newNode) & L.widget .~ w
        w = makeGameControl gcData state'
        state' = if sameCornerRect
            then oldState
            else GameControlState
                { _gcsCornerRect = newCornerRect
                , _gcsOldCornerRect = oldCornerRect'
                , _gcsRunning = animationDuration' > 0
                , _gcsStart = ts
                }
        reqs = (ResizeWidgets widgetId):[reReq | not sameCornerRect]
        reReq = requestRenderEvery newNode animationDuration
        sameCornerRect = newCornerRect == oldCornerRect
        newCornerRect = getCornerRect wenv
        oldCornerRect = _gcsCornerRect oldState
        olderCornerRect = _gcsOldCornerRect oldState
        oldCornerRect' = if delta < animationDuration
            then getMiddleRect olderCornerRect oldCornerRect progress
            else oldCornerRect
        oldStart = _gcsStart oldState
        delta = fromIntegral $ ts-oldStart
        progress = delta/animationDuration
        ts = wenv ^. L.timestamp
        widgetId = newNode ^. L.info . L.widgetId
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
               fmap (getNodeArea . fst) (getNodeSequence grid)
            >< fmap (getLinkArea . fst) (getHlinkSequence grid)
            >< fmap (getLinkArea . fst) (getVlinkSequence grid)
        grid = getGrid wenv
        getNodeArea (i, j) = Rect x y d d where
            x = vx+linkSize*(fromIntegral i)
            y = vy+linkSize*(fromIntegral j)
            d = nodeSize*2
        getLinkArea (i, j) = Rect x y linkSize nodeSize where
            x = vx+linkSize*(fromIntegral i)+nodeSize
            y = vy+linkSize*(fromIntegral j)+nodeSize
        vx = x+(vp ^. L.x)
        vy = y+(vp ^. L.y)
        Rect x y linkSize nodeSize = getCornerRect wenv

    render wenv node renderer = do
        let style = currentStyle wenv node
            vp = getContentArea node style
            ts = wenv ^. L.timestamp
            Rect x y linkSize _ = _gcsCornerRect state
            Rect x' y' linkSize' _ = _gcsOldCornerRect state
            running = _gcsRunning state
            start = _gcsStart state
            delta = fromIntegral $ ts-start
            progress = max 0 $ min 1 $ delta/animationDuration
        saveContext renderer
        when (running && progress < 1) $ do
            let s = (linkSize'/linkSize)*(1-progress)+progress
                tx = (x'-x)*(1-progress)+(1-s)*(x+(vp ^. L.x))
                ty = (y'-y)*(1-progress)+(1-s)*(y+(vp ^. L.y))
            intersectScissor renderer vp
            setTranslation renderer $ Point tx ty
            setScale renderer $ Point s s

    renderAfter _ _ = restoreContext

    makeChildren wenv node = resNode where
        resNode = node & L.children .~ fmap fn nodeSequence
            >< fmap fh hlinkSequence
            >< fmap fv vlinkSequence
        game = widgetDataGet (wenv ^. L.model) gameLens
        grid = gridFromGame game config
        nodeSequence = getNodeSequence grid
        hlinkSequence = getHlinkSequence grid
        vlinkSequence = getVlinkSequence grid
        fn (p, nodeStack) = gameControlNode $ NodeData
            { _ndNodeStack = nodeStack
            , _ndGameControlId = widgetId
            , _ndPosition = p
            , _ndClickable = not $ null tax
            , _ndNextTax = tax
            , _ndNextTaxLens = nextTaxLens
            , _ndConfig = config
            } where tax = taxFromGame p game
        fh = gameControlHlink . linkData
        fv = gameControlVlink . linkData
        linkData (p, link) = LinkData
            { _ldLink = link
            , _ldPosition = p
            , _ldConfig = config
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
        rows = snd $ getBounds $ getGrid wenv

    getCornerRect wenv = Rect x y linkSize nodeSize where
        x = (width-linkSize*factorW)/2
        y = (height-linkSize*factorH)/2
        linkSize = min (width/factorW) (height/factorH)
        nodeSize = linkSize/linkToNodeRatio
        factorW = (fromIntegral cols)+2/linkToNodeRatio
        factorH = (fromIntegral rows)+2/linkToNodeRatio
        (cols, rows) = getBounds $ getGrid wenv

    getMiddleRect rectA rectB progress = rect where
        rect = Rect x y linkSize nodeSize
        x = xA+(xB-xA)*progress
        y = yA+(yB-yA)*progress
        linkSize = linkSizeA+(linkSizeB-linkSizeA)*progress
        nodeSize = nodeSizeA+(nodeSizeB-nodeSizeA)*progress
        Rect xA yA linkSizeA nodeSizeA = rectA
        Rect xB yB linkSizeB nodeSizeB = rectB

    getGrid wenv = gridFromGame game config where
        game = widgetDataGet (wenv ^. L.model) gameLens

    gameLens = WidgetLens $ _gcdGameLens gcData
    nextTaxLens = WidgetLens $ _gcdNextTaxLens gcData
    animationDuration = _gccAnimationDuration config
    linkToNodeRatio = _gccLinkToNodeRatio config
    nodeToWidthRatio = _gccNodeToWidthRatio config
    width = _gccWidth config
    height = _gccHeight config
    config = _gcdConfig gcData