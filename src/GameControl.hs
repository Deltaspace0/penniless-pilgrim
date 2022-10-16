{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module GameControl
    ( GameControlCfg(..)
    , gameControl
    , gameControl_
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Monomer
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

import GameControl.Link
import GameControl.Node
import Game (Game(..))
import Grid

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

makeFields 'Link
makeFields 'Node

gameControl :: Game -> WidgetNode s e
gameControl game = gameControl_ game def

gameControl_ :: Game -> GameControlCfg -> WidgetNode s e
gameControl_ game config = widgetNode where
    widgetNode = defaultWidgetNode "gameControl" widget
    widget = createSingle () def
        { singleHandleEvent = handleEvent
        , singleGetSizeReq  = getSizeReq
        , singleRender      = render
        }
    grid = gridMap nodeTransform linkTransform $ _grid game
    colorLink = _colorLink config
    colorNode = _colorNode config
    linkToNodeRatio  = _linkToNodeRatio config
    nodeToWidthRatio = _nodeToWidthRatio config
    width  = _gcWidth config
    height = _gcHeight config
    (cols, rows) = getBounds grid
    linkSizeW = width/(fromIntegral cols)
    linkSizeH = height/(fromIntegral rows)
    linkSize  = min linkSizeW linkSizeH
    nodeSize  = linkSize/linkToNodeRatio
    linkWidth = nodeSize/nodeToWidthRatio
    handleEvent wenv node target evt = case evt of
        Click p _ _ | isPointInNodeVp node p ->
            Just $ resultReqs node reqs
        _ -> Nothing
        where reqs = [SetFocus $ node ^. L.info . L.widgetId]
    getSizeReq wenv node = (fixedSize width, fixedSize height)
    render wenv node renderer = do
        let style = currentStyle wenv node
            vp = getContentArea node style
        mapM_ (drawHlink renderer vp) $ getHlinkIndices grid
        mapM_ (drawVlink renderer vp) $ getVlinkIndices grid
        mapM_ (drawNode renderer vp)  $ getNodeIndices grid
    drawNode renderer vp (i, j) = do
        let x = vp ^. L.x + linkSize*(fromIntegral i) - nodeSize
            y = vp ^. L.y + linkSize*(fromIntegral j) - nodeSize
            d = nodeSize*2
            node = getNode (i, j) grid
        beginPath renderer
        setFillColor renderer $ if null node
            then colorNode
            else fromMaybe colorNode $ (head node) ^. color
        renderEllipse renderer $ Rect x y d d
        fill renderer
    drawHlink renderer vp (i, j) = do
        let x = vp ^. L.x + linkSize*(fromIntegral i)
            y = vp ^. L.y + linkSize*(fromIntegral j)
            linkM = getHlink (i, j) grid
            link = fromJust linkM
            (color', form') = if null linkM
                then (colorLink, Nothing)
                else (link ^. color, link ^. form)
        beginPath renderer
        setStrokeWidth renderer linkWidth
        setStrokeColor renderer color'
        setFillColor renderer color'
        let x' = x + linkSize - nodeSize
            ars = linkWidth*1.5
        case form' of
            Just LinkBack -> do
                let x'' = x + nodeSize + ars*2
                moveTo renderer $ Point (x'' - 1) y
                renderLineTo renderer $ Point (x' + 1) y
                stroke renderer
                beginPath renderer
                moveTo renderer $ Point (x + nodeSize) y
                renderLineTo renderer $ Point x'' (y - ars)
                renderLineTo renderer $ Point x'' (y + ars)
                closePath renderer
                fill renderer
            Just LinkForward -> do
                let x'' = x' - ars*2
                moveTo renderer $ Point (x + nodeSize - 1) y
                renderLineTo renderer $ Point (x'' + 1) y
                stroke renderer
                beginPath renderer
                moveTo renderer $ Point x' y
                renderLineTo renderer $ Point x'' (y - ars)
                renderLineTo renderer $ Point x'' (y + ars)
                closePath renderer
                fill renderer
            _ -> do
                moveTo renderer $ Point (x + nodeSize - 1) y
                renderLineTo renderer $ Point (x' + 1) y
                stroke renderer
    drawVlink renderer vp (i, j) = do
        let x = vp ^. L.x + linkSize*(fromIntegral i)
            y = vp ^. L.y + linkSize*(fromIntegral j)
            linkM = getVlink (i, j) grid
            link = fromJust linkM
            (color', form') = if null linkM
                then (colorLink, Nothing)
                else (link ^. color, link ^. form)
        beginPath renderer
        setStrokeWidth renderer linkWidth
        setStrokeColor renderer color'
        setFillColor renderer color'
        let y' = y + linkSize - nodeSize
            ars = linkWidth*1.5
        case form' of
            Just LinkBack -> do
                let y'' = y + nodeSize + ars*2
                moveTo renderer $ Point x (y'' - 1)
                renderLineTo renderer $ Point x (y' + 1)
                stroke renderer
                beginPath renderer
                moveTo renderer $ Point x (y + nodeSize)
                renderLineTo renderer $ Point (x + ars) y''
                renderLineTo renderer $ Point (x - ars) y''
                closePath renderer
                fill renderer
            Just LinkForward -> do
                let y'' = y' - ars*2
                moveTo renderer $ Point x (y + nodeSize - 1)
                renderLineTo renderer $ Point x (y'' + 1)
                stroke renderer
                beginPath renderer
                moveTo renderer $ Point x y'
                renderLineTo renderer $ Point (x + ars) y''
                renderLineTo renderer $ Point (x - ars) y''
                closePath renderer
                fill renderer
            _ -> do
                moveTo renderer $ Point x (y + nodeSize - 1)
                renderLineTo renderer $ Point x (y' + 1)
                stroke renderer