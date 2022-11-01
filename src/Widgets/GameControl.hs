{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Widgets.GameControl
    ( GameControlCfg(..)
    , gameControl
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Monomer
import Monomer.Widgets.Container
import qualified Monomer.Lens as L

import Widgets.GameControl.Link
import Widgets.GameControl.Node
import Model.Game hiding (Node, Link, LinkBack, LinkForward)
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

makeFields 'Link

gridFromGame :: Game -> GameControlCfg -> Grid Node Link
gridFromGame game config = gridMap nt lt $ _grid game where
    nt = nodeTransform $ _colors config
    lt = linkTransform $ _colors config

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
        { containerInit        = init
        , containerMerge       = merge
        , containerHandleEvent = handleEvent
        , containerGetSizeReq  = getSizeReq
        , containerRender      = render
        , containerResize      = resize
        }

    init wenv node = resultNode resNode where
        resNode = node
            & L.widget .~ w
            & L.children .~ fmap (f . snd) nodeSequence
        w = makeGameControl field config $ GameControlState grid
        game = widgetDataGet (wenv ^. L.model) field
        grid = gridFromGame game config
        nodeSequence = getNodeSequence grid
        f = flip gameControlNode $ NodeCfg
            { _defaultColor       = _nodeDefault colors
            , _defaultHoverColor  = _nodeHover colors
            , _defaultActiveColor = _nodeActive colors
            }

    merge wenv newNode _ _ = init wenv newNode

    handleEvent wenv node target evt = case evt of
        KeyAction _ code KeyPressed
            | isKeyNorth code -> handleDirection North
            | isKeySouth code -> handleDirection South
            | isKeyWest  code -> handleDirection West
            | isKeyEast  code -> handleDirection East
            where
                isKeyNorth code = isKeyUp    code || isKeyW code
                isKeySouth code = isKeyDown  code || isKeyS code
                isKeyWest  code = isKeyLeft  code || isKeyA code
                isKeyEast  code = isKeyRight code || isKeyD code
        ButtonAction _ _ BtnPressed _ -> Just result where
            result = resultReqs node [SetFocus widgetId]
        _ -> Nothing
        where
            handleDirection direction = Just result where
                result = resultReqs newNode $ RenderOnce:reqs
                newNode = node & L.widget .~ w
                w       = makeGameControl field config state'
                state'  = GameControlState grid
                reqs  = widgetDataSet field game'
                game' = movePilgrim direction game
                game  = widgetDataGet (wenv ^. L.model) field
                grid  = gridFromGame game' config
            widgetId = node ^. L.info . L.widgetId

    getSizeReq wenv node _ = (fixedSize width, fixedSize height)

    render wenv node renderer = do
        let style = currentStyle wenv node
            vp = getContentArea node style
        mapM_ (drawHlink renderer vp) $ getHlinkIndices grid
        mapM_ (drawVlink renderer vp) $ getVlinkIndices grid

    resize wenv node vp children = resized where
        resized = (resultNode node, assignedAreas)
        assignedAreas = (getNodeArea vp . fst) <$> nodeSequence
        nodeSequence = getNodeSequence grid

    drawHlink r vp (i, j) = do
        let x = (vx vp) + linkSize*(fromIntegral i) + nodeSize
            y = (vy vp) + linkSize*(fromIntegral j) + nodeSize
            linkM = getHlink (i, j) grid
            link = fromJust linkM
            (color', form') = if null linkM
                then (colorLink, Nothing)
                else (link ^. color, link ^. form)
            c = Just color'
            x' = x + linkSize - nodeSize
            drawLineH a b = drawLine' r p1 p2 c where
                p1 = Point a y
                p2 = Point b y
            drawTriangleH a b = drawTriangle r p1 p2 p3 c where
                p1 = Point a y
                p2 = Point b $ y - ars
                p3 = Point b $ y + ars
        case form' of
            Just LinkBack -> do
                let x'' = x + nodeSize + ars*2
                drawLineH (x'' - 1) (x' + 1)
                drawTriangleH (x + nodeSize) x''
            Just LinkForward -> do
                let x'' = x' - ars*2
                drawLineH (x + nodeSize - 1) (x'' + 1)
                drawTriangleH x' x''
            _ -> drawLineH (x + nodeSize - 1) (x' + 1)

    drawVlink r vp (i, j) = do
        let x = (vx vp) + linkSize*(fromIntegral i) + nodeSize
            y = (vy vp) + linkSize*(fromIntegral j) + nodeSize
            linkM = getVlink (i, j) grid
            link = fromJust linkM
            (color', form') = if null linkM
                then (colorLink, Nothing)
                else (link ^. color, link ^. form)
            c = Just color'
            y' = y + linkSize - nodeSize
            drawLineV a b = drawLine' r p1 p2 c where
                p1 = Point x a
                p2 = Point x b
            drawTriangleV a b = drawTriangle r p1 p2 p3 c where
                p1 = Point x a
                p2 = Point (x + ars) b
                p3 = Point (x - ars) b
        case form' of
            Just LinkBack -> do
                let y'' = y + nodeSize + ars*2
                drawLineV (y'' - 1) (y' + 1)
                drawTriangleV (y + nodeSize) y''
            Just LinkForward -> do
                let y'' = y' - ars*2
                drawLineV (y + nodeSize - 1) (y'' + 1)
                drawTriangleV y' y''
            _ -> drawLineV (y + nodeSize - 1) (y' + 1)

    drawLine' r a b c = drawLine r a b linkWidth c

    getNodeArea vp (i, j) = Rect x y d d where
        x = (vx vp) + linkSize*(fromIntegral i)
        y = (vy vp) + linkSize*(fromIntegral j)
        d = nodeSize*2

    grid = _gcsGrid state
    colorLink = _linkDefault colors
    colors = _colors config
    linkToNodeRatio  = _linkToNodeRatio config
    nodeToWidthRatio = _nodeToWidthRatio config
    width  = _gcWidth config
    height = _gcHeight config
    (cols, rows) = getBounds grid
    factorW      = (fromIntegral cols)+2/linkToNodeRatio
    factorH      = (fromIntegral rows)+2/linkToNodeRatio
    linkSize     = min (width/factorW) (height/factorH)
    nodeSize     = linkSize/linkToNodeRatio
    linkWidth    = nodeSize/nodeToWidthRatio
    vx vp = (width-linkSize*factorW)/2  + vp ^. L.x
    vy vp = (height-linkSize*factorH)/2 + vp ^. L.y
    ars = linkWidth*1.5