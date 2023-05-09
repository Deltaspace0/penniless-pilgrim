{-# LANGUAGE RecordWildCards #-}

module Widgets.GameControlLink.LinkRenderer
    ( LinkRenderer(..)
    , runRenderer
    ) where

import Control.Lens
import Data.Maybe
import Monomer
import qualified Monomer.Lens as L

import Widgets.GameControlLink.LinkData
import Widgets.GameControlLink.LinkForm
import Widgets.GameControlLink.LinkState
import Widgets.GameControlLink.LinkVisual

data LinkRenderer s e = LinkRenderer
    { _lrEnv :: WidgetEnv s e
    , _lrNode :: WidgetNode s e
    , _lrRenderer :: Renderer
    , _lrLinkData :: LinkData
    , _lrLinkState :: LinkState
    , _lrIsHz :: Bool
    }

runRenderer :: LinkRenderer s e -> IO ()
runRenderer linkRenderer@(LinkRenderer{..}) = do
    let LinkData{..} = _lrLinkData
        LinkState{..} = _lrLinkState
        delta = fromIntegral $ (_lrEnv ^. L.timestamp)-_lsStart
        progress = max 0 $ min 1 $ delta/_ldAnimationDuration
        empty = LinkVisual undefined LinkDefault
        new = fromMaybe empty $ _lsLink
        old = fromMaybe empty $ _lsOldLink
        newForm = _linkForm new
        oldForm = _linkForm old
    if _lsRunning && progress < 1
        then if progress < 0.5
            then renderForm linkRenderer old newForm $ 1-progress*2
            else renderForm linkRenderer new oldForm $ progress*2-1
        else renderForm linkRenderer new oldForm 1

renderForm
    :: LinkRenderer s e
    -> LinkVisual
    -> LinkForm
    -> Double
    -> IO ()
renderForm LinkRenderer{..} new oldForm progress = do
    let LinkData{..} = _lrLinkData
        r = _lrRenderer
        style = currentStyle _lrEnv _lrNode
        Rect x y linkSize nodeSize = getContentArea _lrNode style
        LinkVisual color' form' = new
        c = Just color'
        totalSize = linkSize-nodeSize*2
        totalSize' = totalSize*progress
        rest = totalSize-totalSize'
        s' = nodeSize + if _lrIsHz then x else y
        f a = if a == LinkBack then rest else 0
        s = s' + if form' == LinkDefault then f oldForm else f form'
        b = s+totalSize'
        linkWidth = nodeSize/_ldNodeToWidthRatio
        ars = if totalSize' > linkWidth*3
            then linkWidth*1.5
            else totalSize'/2
        drawLine' a a1 = drawLine r p1 p2 linkWidth c where
            p1 = if _lrIsHz then Point a y else Point x a
            p2 = if _lrIsHz then Point a1 y else Point x a1
        drawTriangle' a a1 = drawTriangle r p1 p2 p3 c where
            p1 = if _lrIsHz then Point a y else Point x a
            p2 = if _lrIsHz then Point a1 y1 else Point x1 a1
            p3 = if _lrIsHz then Point a1 y2 else Point x2 a1
            (x1, x2) = (x+ars, x-ars)
            (y1, y2) = (y-ars, y+ars)
    case form' of
        LinkBack -> do
            let b' = s+ars*2
            drawLine' (b'-1) b
            drawTriangle' s b'
        LinkForward -> do
            let b' = b-ars*2
            drawLine' s (b'+1)
            drawTriangle' b b'
        LinkDefault -> drawLine' s b
