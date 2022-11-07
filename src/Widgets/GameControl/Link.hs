{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Control.Monad
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

data LinkData = LinkData
    { _ldLink :: Maybe Link
    , _ldColor :: Color
    , _ldAnimationDuration :: Double
    , _ldNodeToWidthRatio :: Double
    } deriving (Eq, Show)

data LinkState = LinkState
    { _lsLink :: Maybe Link
    , _lsOldLink :: Maybe Link
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
    node = defaultWidgetNode "gameControlLink" widget
    widget = makeGameControlLink True linkData def

gameControlVlink :: LinkData -> WidgetNode s e
gameControlVlink linkData = node where
    node = defaultWidgetNode "gameControlLink" widget
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
                , _lsRunning = True
                , _lsStart = newStart
                }
        reqs = if newLink == oldLink
            then []
            else [RenderEvery widgetId period (Just steps)]
        newLink = _ldLink linkData
        oldLink = _lsLink oldState
        oldStart = _lsStart oldState
        delta = min animationDuration' (ts-oldStart)
        newStart = ts - animationDuration' + delta
        ts = wenv ^. L.timestamp
        widgetId = newNode ^. L.info . L.widgetId
        period = 10
        steps = fromIntegral $ animationDuration' `div` period
        animationDuration' = floor animationDuration

    render wenv node r = do
        let ts = wenv ^. L.timestamp
            newLink = _lsLink state
            oldLink = _lsOldLink state
            running = _lsRunning state
            start = _lsStart state
            style = currentStyle wenv node
            contentArea = getContentArea node style
            delta = fromIntegral $ ts-start
            progress = max 0 $ min 1 $ delta/animationDuration
            Just newLink' = newLink
            Just oldLink' = oldLink
            newData@(newColor, newForm) = if null newLink
                then (_ldColor linkData, Nothing)
                else (newLink' ^. color, newLink' ^. form)
            oldData@(oldColor, oldForm) = if null oldLink
                then (_ldColor linkData, Nothing)
                else (oldLink' ^. color, oldLink' ^. form)
        if running && progress < 1
            then if null newForm
                then renderForm r oldData contentArea (1-progress)
                else renderForm r newData contentArea progress
            else renderForm r newData contentArea 1

    renderForm r (color', form') contentArea progress = do
        let Rect x y linkSize nodeSize = contentArea
            c = Just color'
            totalSize = linkSize-nodeSize*2
            totalSize' = totalSize*progress
            rest = totalSize-totalSize'
            s' = nodeSize + if isHz then x else y
            s = s' + if form' == Just LinkBack then rest else 0
            b = s+totalSize'
            linkWidth = nodeSize/(_ldNodeToWidthRatio linkData)
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

    animationDuration = _ldAnimationDuration linkData