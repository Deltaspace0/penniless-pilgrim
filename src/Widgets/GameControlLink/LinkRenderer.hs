module Widgets.GameControlLink.LinkRenderer
    ( LinkRenderer(..)
    , runRenderer
    ) where

import Control.Lens
import Monomer
import qualified Monomer.Lens as L

import Model.Game
import Widgets.GameControl.GameControlConfig
import Widgets.GameControlLink.LinkData
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
runRenderer linkRenderer = do
    let wenv = _lrEnv linkRenderer
        node = _lrNode linkRenderer
        linkData = _lrLinkData linkRenderer
        state = _lrLinkState linkRenderer
        ts = wenv ^. L.timestamp
        newLink = _lsLink state
        oldLink = _lsOldLink state
        delta = fromIntegral $ ts-(_lsStart state)
        animationDuration = getAnimationDuration linkData
        progress = max 0 $ min 1 $ delta/animationDuration
        Just newLink' = newLink
        Just oldLink' = oldLink
        colorConfig = getLinkColorConfig linkData
        new@(newColor, newForm) = if null newLink
            then (_lccDefault colorConfig, Nothing)
            else (_linkColor newLink', _linkForm newLink')
        old@(oldColor, oldForm) = if null oldLink
            then (_lccDefault colorConfig, Nothing)
            else (_linkColor oldLink', _linkForm oldLink')
    if _lsRunning state && progress < 1
        then if progress < 0.5
            then renderForm linkRenderer old new $ 1-progress*2
            else renderForm linkRenderer new old $ progress*2-1
        else renderForm linkRenderer new old 1

renderForm
    :: LinkRenderer s e
    -> (Color, Maybe GameLink)
    -> (Color, Maybe GameLink)
    -> Double
    -> IO ()
renderForm linkRenderer (color', form') (_, form'') progress = do
    let wenv = _lrEnv linkRenderer
        node = _lrNode linkRenderer
        renderer = _lrRenderer linkRenderer
        linkData = _lrLinkData linkRenderer
        isHz = _lrIsHz linkRenderer
        style = currentStyle wenv node
        vp = getContentArea node style
        Rect x y linkSize nodeSize = vp
        c = Just color'
        totalSize = linkSize-nodeSize*2
        totalSize' = totalSize*progress
        rest = totalSize-totalSize'
        s' = nodeSize + if isHz then x else y
        f x = if x == Just LinkBack then rest else 0
        s = s' + if null form' then f form'' else f form'
        b = s+totalSize'
        nodeToWidthRatio = _gccNodeToWidthRatio $ _ldConfig linkData
        linkWidth = nodeSize/nodeToWidthRatio
        ars = if totalSize' > linkWidth*3
            then linkWidth*1.5
            else totalSize'/2
        drawLine' a b = drawLine renderer p1 p2 linkWidth c where
            p1 = if isHz then Point a y else Point x a
            p2 = if isHz then Point b y else Point x b
        drawTriangle' a b = drawTriangle renderer p1 p2 p3 c where
            p1 = if isHz then Point a y else Point x a
            p2 = if isHz then Point b y1 else Point x1 b
            p3 = if isHz then Point b y2 else Point x2 b
            x1 = x+ars
            x2 = x-ars
            y1 = y-ars
            y2 = y+ars
    case form' of
        Just LinkBack -> do
            let b' = s+ars*2
            drawLine' (b'-1) b
            drawTriangle' s b'
        Just LinkForward -> do
            let b' = b-ars*2
            drawLine' s (b'+1)
            drawTriangle' b b'
        _ -> drawLine' s b
