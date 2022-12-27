module Widgets.GameControlNode.NodeRenderer
    ( NodeRenderer(..)
    , runRenderer
    ) where

import Control.Lens
import Data.Maybe
import Monomer
import qualified Monomer.Lens as L

import Widgets.GameControl.GameControlConfig
import Widgets.GameControlNode.NodeData
import Widgets.GameControlNode.NodeState
import Widgets.GameControlNode.NodeVisual

data NodeRenderer s e = NodeRenderer
    { _nrEnv :: WidgetEnv s e
    , _nrNode :: WidgetNode s e
    , _nrRenderer :: Renderer
    , _nrNodeData :: NodeData s
    , _nrNodeState :: NodeState
    }

runRenderer :: NodeRenderer s e -> IO ()
runRenderer nodeRenderer = do
    let wenv = _nrEnv nodeRenderer
        node = _nrNode nodeRenderer
        renderer = _nrRenderer nodeRenderer
        nodeData = _nrNodeData nodeRenderer
        state = _nrNodeState nodeRenderer
        ts = wenv ^. L.timestamp
        visualStates = reverse $ _nsVisualStates state
        style = currentStyle wenv node
        vp = getShakeArea nodeRenderer
        animationDuration = getAnimationDuration nodeData
    if null visualStates || floor animationDuration == 0
        then drawEllipse renderer vp $ _sstFgColor style
        else mapM_ (renderVisual nodeRenderer vp ts) visualStates
    let visualStack = _ndVisualStack nodeData
        colorConfig = getDefaultNodeColors nodeData
        visual = head visualStack
        highlightColor
            | not $ _ndClickable nodeData = Nothing
            | null visualStack = Just $ _nodeHighlight colorConfig
            | otherwise = Just $ _nodeColorHighlight visual
    drawEllipseBorder renderer vp highlightColor 2

getShakeArea :: NodeRenderer s e -> Rect
getShakeArea nodeRenderer = shakeArea where
    shakeArea = if _ssRunning $ _nsShakeState state
        then Rect (x+dx) (y+dy) (w-dx*2) (h-dy*2)
        else vp
    wenv = _nrEnv nodeRenderer
    node = _nrNode nodeRenderer
    nodeData = _nrNodeData nodeRenderer
    state = _nrNodeState nodeRenderer
    vp@(Rect x y w h) = getContentArea node style
    dx = w*sf/2
    dy = h*sf/2
    sf = (1+(sin $ progress*pi*7/2))/5
    progress = max 0 $ min 1 $ delta/animationDuration
    delta = fromIntegral $ ts-(_ssStart $ _nsShakeState state)
    style = currentStyle wenv node
    animationDuration = getAnimationDuration nodeData
    ts = wenv ^. L.timestamp

renderVisual
    :: NodeRenderer s e
    -> Rect
    -> Millisecond
    -> VisualState
    -> IO ()
renderVisual nodeRenderer vp ts visualState = do
    let wenv = _nrEnv nodeRenderer
        node = _nrNode nodeRenderer
        renderer = _nrRenderer nodeRenderer
        nodeData = _nrNodeData nodeRenderer
        VisualState visual start = visualState
        Rect x y w h = vp
        delta = fromIntegral $ ts-start
        animationDuration = getAnimationDuration nodeData
        progress = max 0 $ min 1 $ delta/animationDuration
        dx = w*(1-progress)/2
        dy = h*(1-progress)/2
        vp' = Rect (x+dx) (y+dy) (w-dx*2) (h-dy*2)
        clickable = _ndClickable nodeData
        isActive = clickable && isNodeActive wenv node
        isHovered = clickable && isNodeHovered wenv node
        color = Just $ getColor nodeData isActive isHovered visual
    drawEllipse renderer vp' color

getColor :: NodeData s -> Bool -> Bool -> Maybe NodeVisual -> Color
getColor nodeData isActive isHovered visual
    | isActive = activeColor
    | isHovered = hoverColor
    | otherwise = defaultColor
    where
        activeColor = if null visual
            then _nodeActive colorConfig
            else _nodeColorActive $ fromJust visual
        hoverColor = if null visual
            then _nodeHover colorConfig
            else _nodeColorHover $ fromJust visual
        defaultColor = if null visual
            then _nodeDefault colorConfig
            else _nodeColorDefault $ fromJust visual
        colorConfig = getDefaultNodeColors nodeData