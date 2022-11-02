{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Widgets.GameControl.Link
    ( LinkForm(..)
    , Link(..)
    , LinkData(..)
    , hlinkTransform
    , vlinkTransform
    , gameControlHlink
    , gameControlVlink
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Monomer
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

import Model.Parameters.Colors
import qualified Model.Game as G

data LinkForm = LinkBack | LinkForward deriving (Eq, Show)

data Link = Link
    { _linkColor :: Color
    , _linkForm :: Maybe LinkForm
    } deriving (Eq, Show)

data LinkData = LinkData
    { _ldLink :: Maybe Link
    , _ldColor :: Color
    , _ldNodeToWidthRatio :: Double
    } deriving (Eq, Show)

makeFields 'Link

hlinkTransform :: Colors -> Maybe G.Link -> Maybe Link
hlinkTransform _ Nothing = Nothing
hlinkTransform colors (Just G.LinkBack) = Just Link
    { _linkColor = _linkWest colors
    , _linkForm = Just LinkBack
    }
hlinkTransform colors (Just G.LinkForward) = Just Link
    { _linkColor = _linkEast colors
    , _linkForm = Just LinkForward
    }

vlinkTransform :: Colors -> Maybe G.Link -> Maybe Link
vlinkTransform _ Nothing = Nothing
vlinkTransform colors (Just G.LinkBack) = Just Link
    { _linkColor = _linkNorth colors
    , _linkForm = Just LinkBack
    }
vlinkTransform colors (Just G.LinkForward) = Just Link
    { _linkColor = _linkSouth colors
    , _linkForm = Just LinkForward
    }

gameControlHlink :: LinkData -> WidgetNode s e
gameControlHlink linkData = node where
    node = defaultWidgetNode "gameControlLink" widget
    widget = makeGameControlLink True linkData

gameControlVlink :: LinkData -> WidgetNode s e
gameControlVlink linkData = node where
    node = defaultWidgetNode "gameControlLink" widget
    widget = makeGameControlLink False linkData

makeGameControlLink :: Bool -> LinkData -> Widget s e
makeGameControlLink isHz linkData = widget where
    widget = createSingle () def
        { singleRender = render
        }

    render wenv node r = do
        let link = _ldLink linkData
            style = currentStyle wenv node
            Rect x y linkSize nodeSize = getContentArea node style
            Just link' = link
            (color', form') = if null link
                then (_ldColor linkData, Nothing)
                else (link' ^. color, link' ^. form)
            c = Just color'
            s = if isHz then x else y
            b = s + linkSize - nodeSize
            linkWidth = nodeSize/(_ldNodeToWidthRatio linkData)
            ars = linkWidth*1.5
            drawLine' a b = drawLine r p1 p2 linkWidth c where
                p1 = if isHz then Point a y else Point x a
                p2 = if isHz then Point b y else Point x b
            drawTriangle' a b = drawTriangle r p1 p2 p3 c where
                p1 = if isHz then Point a y else Point x a
                p2 = if isHz then Point b y1 else Point x1 b
                p3 = if isHz then Point b y2 else Point x2 b
                x1 = x + ars
                x2 = x - ars
                y1 = y - ars
                y2 = y + ars
        case form' of
            Just LinkBack -> do
                let b' = s + nodeSize + ars*2
                drawLine' (b' - 1) (b + 1)
                drawTriangle' (s + nodeSize) b'
            Just LinkForward -> do
                let b' = b - ars*2
                drawLine' (s + nodeSize - 1) (b' + 1)
                drawTriangle' b b'
            _ -> drawLine' (s + nodeSize - 1) (b + 1)