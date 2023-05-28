{-# LANGUAGE RecordWildCards #-}

module Widgets.GameControl.UI
    ( buildUI
    ) where

import Control.Lens
import Data.Foldable
import Data.Maybe
import Data.Sequence ((><))
import Data.Tuple
import Monomer
import Monomer.Graph
import TextShow
import qualified Monomer.Lens as L

import Common
import Widgets.GameControl.ControlledGame
import Widgets.GameControl.GameControlCfg
import Widgets.GameControl.GameControlEvent
import Widgets.GameControl.GameControlModel
import Widgets.GameControl.LinkVisual
import Widgets.GameControl.NodeVisual

buildUI
    :: (CompositeModel a, ControlledGame a)
    => (a -> Grid NodeVisual LinkVisual)
    -> GameControlCfg s
    -> UIBuilder (GameControlModel a) GameControlEvent
buildUI getGrid config _ GameControlModel{..} = uiNode where
    uiNode = if null _gcmControlledGame
        then spacer
        else node
    node = keystroke keyEvents $ focus $ graphWithData_ graphDatas
        [ hideGrid
        , minimumX 0
        , maximumY 0
        , minScale 0.5
        , maxScale 5
        , wheelRate 2
        ] `styleBasic` [bgColor $ rgb 34 36 42]
    keyEvents =
        [ ("Left", EventDirection West)
        , ("Right", EventDirection East)
        , ("Up", EventDirection North)
        , ("Down", EventDirection South)
        , ("a", EventDirection West)
        , ("d", EventDirection East)
        , ("w", EventDirection North)
        , ("s", EventDirection South)
        ]
    focus = L.info . L.focusable .~ True
    graphDatas = toList $ hlinks >< vlinks >< nodes
    nodes = fn <$> getNodeSequence grid
    hlinks = fl True <$> getHlinkSequence grid
    vlinks = fl False <$> getVlinkSequence grid
    fn (p, visualStack) = graphData where
        graphData =
            [ graphPoint $ negateY $ shiftPoint p
            , graphRadius $ if _gcmShakeNode /= Just p
                then nodeRadius
                else nodeRadius*0.6
            , graphWidth 4
            , graphDuration dur
            , graphColor _nodeColorDefault
            , graphHoverColor _nodeColorHover
            , graphActiveColor _nodeColorActive
            , graphBorderColor borderColor
            , graphSeparate_ $ not $ null score
            , graphOnEnter $ const $ EventSetScore score
            , graphOnLeave $ const $ EventSetScore Nothing
            , graphOnClick $ const $ EventClick p
            , graphKey $ "node" <> (showt p)
            , if _gcmShakeNode /= Just p
                then mempty
                else mconcat
                    [ graphOnFinished EventStopShake
                    , graphAnimationTwist twist
                    ]
            ]
        borderColor = if null score
            then transparent
            else _nodeColorHighlight
        NodeVisual{..} = if null visualStack
            then getDefaultNodeVisual config
            else head visualStack
        score = getScoreByPosition p game
        twist t = (1+(sin $ t*pi*7/2))/2
    fl isHorizontal (p, link) = graphData where
        graphData =
            [ graphPoints ps
            , graphDuration dur
            , graphColor _linkColor
            , graphFill
            , graphFillAlpha 1
            , graphKey $ "link" <> (showt isHorizontal) <> (showt p)
            , graphWidth 1
            ]
        ps = negateY <$> getLinkPoints isHorizontal p _linkForm
        LinkVisual{..} = fromMaybe emptyLink link
        emptyLink = LinkVisual transparent LinkDefault
    negateY (i, j) = (i, -j)
    shiftPoint (i, j) = (1+fromIntegral i, 1+fromIntegral j)
    getLinkPoints isHorizontal p linkForm = linkPoints where
        linkPoints = if isHorizontal
            then horizontalPoints
            else swap <$> getLinkPoints True (swap p) linkForm
        horizontalPoints = case linkForm of
            LinkBack ->
                [ (i+nodeRadius, j)
                , (i+nodeRadius+ars*2, j-ars)
                , (i+nodeRadius+ars*2, j-linkWidth/2)
                , (i+nodeRadius+q, j-linkWidth/2)
                , (i+nodeRadius+q, j-linkWidth/2)
                , (i-nodeRadius+1, j-linkWidth/2)
                , (i-nodeRadius+1, j+linkWidth/2)
                , (i+nodeRadius+q, j+linkWidth/2)
                , (i+nodeRadius+q, j+linkWidth/2)
                , (i+nodeRadius+ars*2, j+linkWidth/2)
                , (i+nodeRadius+ars*2, j+ars)
                , (i+nodeRadius, j)
                ] where q = max (ars*2) (1-ars*2-nodeRadius*2)
            LinkForward ->
                [ (i+nodeRadius, j-linkWidth/2)
                , (i-nodeRadius+q, j-linkWidth/2)
                , (i-nodeRadius+q, j-linkWidth/2)
                , (i-nodeRadius+1-ars*2, j-linkWidth/2)
                , (i-nodeRadius+1-ars*2, j-ars)
                , (i-nodeRadius+1, j)
                , (i-nodeRadius+1, j)
                , (i-nodeRadius+1-ars*2, j+ars)
                , (i-nodeRadius+1-ars*2, j+linkWidth/2)
                , (i-nodeRadius+q, j+linkWidth/2)
                , (i-nodeRadius+q, j+linkWidth/2)
                , (i+nodeRadius, j+linkWidth/2)
                ] where q = min (ars*2+nodeRadius*2) (1-ars*2)
            LinkDefault ->
                [ (i+nodeRadius, j-linkWidth/2)
                , (i+nodeRadius+ars*2, j-linkWidth/2)
                , (i+nodeRadius+ars*2, j-linkWidth/2)
                , (i-nodeRadius+1-ars*2, j-linkWidth/2)
                , (i-nodeRadius+1-ars*2, j-linkWidth/2)
                , (i-nodeRadius+1, j-linkWidth/2)
                , (i-nodeRadius+1, j+linkWidth/2)
                , (i-nodeRadius+1-ars*2, j+linkWidth/2)
                , (i-nodeRadius+1-ars*2, j+linkWidth/2)
                , (i+nodeRadius+ars*2, j+linkWidth/2)
                , (i+nodeRadius+ars*2, j+linkWidth/2)
                , (i+nodeRadius, j+linkWidth/2)
                ]
        (i, j) = shiftPoint p
    ars = linkWidth*1.5
    linkWidth = nodeRadius/(getNodeToWidthRatio config)
    nodeRadius = 1/(getLinkToNodeRatio config)
    grid = getGrid game
    dur = round $ getAnimationDuration config
    game = fromJust _gcmControlledGame