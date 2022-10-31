{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Widgets.GameControl
    ( GameControlCfg(..)
    , gameControl
    , gameControl_
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Monomer
import Monomer.Widgets.Single
import Monomer.Widgets.Util.Drawing
import qualified Monomer.Lens as L

import Widgets.GameControl.Link
import Widgets.GameControl.Node
import Model.Game hiding (Node, Link, LinkBack, LinkForward)
import Model.Grid

data GameControlCfg = GameControlCfg
    { _colorLink :: Color
    , _colorNode :: Color
    , _linkToNodeRatio  :: Double
    , _nodeToWidthRatio :: Double
    , _gcWidth  :: Double
    , _gcHeight :: Double
    } deriving (Eq, Show)

instance Default GameControlCfg where
    def = GameControlCfg
        { _colorLink = darkGray
        , _colorNode = darkGray
        , _linkToNodeRatio  = 5
        , _nodeToWidthRatio = 3
        , _gcWidth  = 400
        , _gcHeight = 500
        }

newtype GameControlState = GameControlState
    { _gcsGrid :: Grid Node Link
    }

makeFields 'Link
makeFields 'Node

gridFromGame :: Game -> Grid Node Link
gridFromGame = gridMap nodeTransform linkTransform . _grid

gameControl :: ALens' s Game -> WidgetNode s e
gameControl field = gameControl_ field def

gameControl_ :: ALens' s Game -> GameControlCfg -> WidgetNode s e
gameControl_ field config = gameControlNode where
    gameControlNode = defaultWidgetNode "gameControl" widget
    widget = makeGameControl (WidgetLens field) config state
    state = GameControlState $ makeGrid 2 2

makeGameControl
    :: WidgetData s Game
    -> GameControlCfg
    -> GameControlState
    -> Widget s e
makeGameControl field config state = widget where
    widget = createSingle state def
        { singleInit        = init
        , singleMerge       = merge
        , singleHandleEvent = handleEvent
        , singleGetSizeReq  = getSizeReq
        , singleRender      = render
        }

    init wenv node = resultNode resNode where
        resNode = node & L.widget .~ w
        w = makeGameControl field config state'
        state' = GameControlState $ gridFromGame game
        game = widgetDataGet (wenv ^. L.model) field
    
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
                state'  = GameControlState $ gridFromGame game'
                reqs  = widgetDataSet field game'
                game' = movePilgrim direction game
                game  = widgetDataGet (wenv ^. L.model) field
            widgetId = node ^. L.info . L.widgetId

    getSizeReq wenv node = (fixedSize width, fixedSize height)

    render wenv node renderer = do
        let style = currentStyle wenv node
            vp = getContentArea node style
        mapM_ (drawHlink renderer vp) $ getHlinkIndices grid
        mapM_ (drawVlink renderer vp) $ getVlinkIndices grid
        mapM_ (drawNode renderer vp)  $ getNodeIndices grid

    drawNode renderer vp (i, j) = do
        let x = (vx vp) + linkSize*(fromIntegral i)
            y = (vy vp) + linkSize*(fromIntegral j)
            d = nodeSize*2
            node = getNode (i, j) grid
        drawEllipse renderer (Rect x y d d) $ Just $ if null node
            then colorNode
            else fromMaybe colorNode $ (head node) ^. color

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

    grid = _gcsGrid state
    colorLink        = _colorLink config
    colorNode        = _colorNode config
    linkToNodeRatio  = _linkToNodeRatio config
    nodeToWidthRatio = _nodeToWidthRatio config
    width            = _gcWidth config
    height           = _gcHeight config
    (cols, rows) = getBounds grid
    factorW      = (fromIntegral cols)+2/linkToNodeRatio
    factorH      = (fromIntegral rows)+2/linkToNodeRatio
    linkSize     = min (width/factorW) (height/factorH)
    nodeSize     = linkSize/linkToNodeRatio
    linkWidth    = nodeSize/nodeToWidthRatio
    vx vp = (width-linkSize*factorW)/2  + vp ^. L.x
    vy vp = (height-linkSize*factorH)/2 + vp ^. L.y
    ars = linkWidth*1.5