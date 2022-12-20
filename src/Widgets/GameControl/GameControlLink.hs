{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControl.GameControlLink
    ( module Widgets.GameControl.GameControlLink.LinkVisual
    , LinkData(..)
    , gameControlHlink
    , gameControlVlink
    ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Monomer
import Monomer.Widgets.Single
import TextShow
import qualified Monomer.Lens as L

import Model.Game
import Util
import Widgets.GameControl.GameControlConfig
import Widgets.GameControl.GameControlLink.LinkVisual

data LinkData = LinkData
    { _ldLink :: Maybe LinkVisual
    , _ldPosition :: (Int, Int)
    , _ldConfig :: GameControlConfig
    } deriving (Eq, Show)

data LinkState = LinkState
    { _lsLink :: Maybe LinkVisual
    , _lsOldLink :: Maybe LinkVisual
    , _lsRunning :: Bool
    , _lsStart :: Millisecond
    } deriving (Eq, Show)

instance Default LinkState where
    def = LinkState
        { _lsLink = Nothing
        , _lsOldLink = Nothing
        , _lsRunning = False
        , _lsStart = 0
        }

gameControlHlink :: LinkData -> WidgetNode s e
gameControlHlink linkData = node where
    node = defaultWidgetNode (WidgetType widgetType) widget
    widgetType = "gameControlHLink" <> showt position
    position = _ldPosition linkData
    widget = makeGameControlLink True linkData def

gameControlVlink :: LinkData -> WidgetNode s e
gameControlVlink linkData = node where
    node = defaultWidgetNode (WidgetType widgetType) widget
    widgetType = "gameControlVLink" <> showt position
    position = _ldPosition linkData
    widget = makeGameControlLink False linkData def

makeGameControlLink :: Bool -> LinkData -> LinkState -> Widget s e
makeGameControlLink isHz linkData state = widget where
    widget = createSingle state def
        { singleInit = init
        , singleMerge = merge
        , singleRender = render
        }

    init _ node = resultNode resNode where
        resNode = node & L.widget .~ w
        w = makeGameControlLink isHz linkData state'
        state' = state
            { _lsLink = _ldLink linkData
            , _lsOldLink = _ldLink linkData
            }

    merge wenv newNode _ oldState = result where
        result = resultReqs resNode reqs
        resNode = newNode & L.widget .~ w
        w = makeGameControlLink isHz linkData state'
        state' = if newLink == oldLink
            then oldState
            else LinkState
                { _lsLink = newLink
                , _lsOldLink = oldLink
                , _lsRunning = animationDuration' > 0
                , _lsStart = newStart
                }
        reqs = if newLink == oldLink
            then []
            else [requestRenderEvery newNode animationDuration]
        newLink = _ldLink linkData
        oldLink = _lsLink oldState
        oldStart = _lsStart oldState
        delta = min animationDuration' (ts-oldStart)
        newStart = ts - animationDuration' + delta
        ts = wenv ^. L.timestamp
        animationDuration' = floor animationDuration

    render wenv node r = do
        let ts = wenv ^. L.timestamp
            newLink = _lsLink state
            oldLink = _lsOldLink state
            running = _lsRunning state
            start = _lsStart state
            style = currentStyle wenv node
            vp = getContentArea node style
            delta = fromIntegral $ ts-start
            progress = max 0 $ min 1 $ delta/animationDuration
            Just newLink' = newLink
            Just oldLink' = oldLink
            newData@(newColor, newForm) = if null newLink
                then (_lccDefault colorConfig, Nothing)
                else (_linkColor newLink', _linkForm newLink')
            oldData@(oldColor, oldForm) = if null oldLink
                then (_lccDefault colorConfig, Nothing)
                else (_linkColor oldLink', _linkForm oldLink')
        if running && progress < 1
            then if progress < 0.5
                then renderForm r oldData newData vp $ 1-progress*2
                else renderForm r newData oldData vp $ progress*2-1
            else renderForm r newData oldData vp 1

    renderForm r (color', form') (_, form'') vp progress = do
        let Rect x y linkSize nodeSize = vp
            c = Just color'
            totalSize = linkSize-nodeSize*2
            totalSize' = totalSize*progress
            rest = totalSize-totalSize'
            s' = nodeSize + if isHz then x else y
            f x = if x == Just LinkBack then rest else 0
            s = s' + if null form' then f form'' else f form'
            b = s+totalSize'
            linkWidth = nodeSize/(_gccNodeToWidthRatio config)
            ars = if totalSize' > linkWidth*3
                then linkWidth*1.5
                else totalSize'/2
            drawLine' a b = drawLine r p1 p2 linkWidth c where
                p1 = if isHz then Point a y else Point x a
                p2 = if isHz then Point b y else Point x b
            drawTriangle' a b = drawTriangle r p1 p2 p3 c where
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

    animationDuration = _gccAnimationDuration config
    colorConfig = _gcccLink $ _gccColorConfig config
    config = _ldConfig linkData