{-# LANGUAGE RecordWildCards #-}

module Widgets.GameControlNode.NodeRenderer
    ( NodeRenderer(..)
    , runRenderer
    ) where

import Control.Lens
import Data.Maybe
import Monomer
import qualified Monomer.Lens as L

import Widgets.ButtonColors
import Widgets.GameControlNode.NodeData
import Widgets.GameControlNode.NodeState
import Widgets.GameControlNode.NodeVisual

data NodeRenderer s e c = NodeRenderer
    { _nrEnv :: WidgetEnv s e
    , _nrNode :: WidgetNode s e
    , _nrRenderer :: Renderer
    , _nrNodeData :: NodeData s c
    , _nrNodeState :: NodeState
    }

runRenderer :: (ButtonColors c) => NodeRenderer s e c -> IO ()
runRenderer nodeRenderer@(NodeRenderer{..}) = do
    let NodeData{..} = _nrNodeData
        NodeState{..} = _nrNodeState
        ts = _nrEnv ^. L.timestamp
        visualStates = reverse _nsVisualStates
        style = currentStyle _nrEnv _nrNode
        vp = getShakeArea nodeRenderer
    if null visualStates || (floor _ndAnimationDuration :: Int) == 0
        then drawEllipse _nrRenderer vp $ _sstFgColor style
        else mapM_ (renderVisual nodeRenderer vp ts) visualStates
    let config = _ndDefaultColors
        visual = head _ndVisualStack
        highlightColor
            | not _ndClickable = Nothing
            | null _ndVisualStack = Just $ getHighlightColor config
            | otherwise = Just $ _nodeColorHighlight visual
    drawEllipseBorder _nrRenderer vp highlightColor 2

getShakeArea :: NodeRenderer s e c -> Rect
getShakeArea NodeRenderer{..} = shakeArea where
    shakeArea = if _ssRunning
        then Rect (x+dx) (y+dy) (w-dx*2) (h-dy*2)
        else vp
    vp@(Rect x y w h) = getContentArea _nrNode style
    (dx, dy) = (w*sf/2, h*sf/2)
    sf = (1+(sin $ progress*pi*7/2))/5
    progress = max 0 $ min 1 $ delta/_ndAnimationDuration
    delta = fromIntegral $ (_nrEnv ^. L.timestamp)-_ssStart
    style = currentStyle _nrEnv _nrNode
    ShakeState{..} = _nsShakeState
    NodeData{..} = _nrNodeData
    NodeState{..} = _nrNodeState

renderVisual
    :: (ButtonColors c)
    => NodeRenderer s e c
    -> Rect
    -> Millisecond
    -> VisualState
    -> IO ()
renderVisual NodeRenderer{..} vp ts visualState = do
    let NodeData{..} = _nrNodeData
        VisualState visual start = visualState
        Rect x y w h = vp
        delta = fromIntegral $ ts-start
        progress = max 0 $ min 1 $ delta/_ndAnimationDuration
        (dx, dy) = (w*(1-progress)/2, h*(1-progress)/2)
        vp' = Rect (x+dx) (y+dy) (w-dx*2) (h-dy*2)
        isActive = _ndClickable && isNodeActive _nrEnv _nrNode
        isHovered = _ndClickable && isNodeHovered _nrEnv _nrNode
        color = getColor _ndDefaultColors isActive isHovered visual
    drawEllipse _nrRenderer vp' $ Just color

getColor
    :: (ButtonColors a, ButtonColors b)
    => a
    -> Bool
    -> Bool
    -> Maybe b
    -> Color
getColor config isActive isHovered visual
    | isActive = fromMaybe (getActiveColor config) visualActive
    | isHovered = fromMaybe (getHoverColor config) visualHover
    | otherwise = fromMaybe (getDefaultColor config) visualDefault
    where
        visualActive = getActiveColor <$> visual
        visualHover = getHoverColor <$> visual
        visualDefault = getDefaultColor <$> visual
