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
        animationDuration = _ndAnimationDuration nodeData
    if null visualStates || (floor animationDuration :: Int) == 0
        then drawEllipse renderer vp $ _sstFgColor style
        else mapM_ (renderVisual nodeRenderer vp ts) visualStates
    let visualStack = _ndVisualStack nodeData
        config = _ndDefaultColors nodeData
        visual = head visualStack
        highlightColor
            | not $ _ndClickable nodeData = Nothing
            | null visualStack = Just $ getHighlightColor config
            | otherwise = Just $ _nodeColorHighlight visual
    drawEllipseBorder renderer vp highlightColor 2

getShakeArea :: NodeRenderer s e c -> Rect
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
    animationDuration = _ndAnimationDuration nodeData
    ts = wenv ^. L.timestamp

renderVisual
    :: (ButtonColors c)
    => NodeRenderer s e c
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
        animationDuration = _ndAnimationDuration nodeData
        progress = max 0 $ min 1 $ delta/animationDuration
        dx = w*(1-progress)/2
        dy = h*(1-progress)/2
        vp' = Rect (x+dx) (y+dy) (w-dx*2) (h-dy*2)
        clickable = _ndClickable nodeData
        isActive = clickable && isNodeActive wenv node
        isHovered = clickable && isNodeHovered wenv node
        config = _ndDefaultColors nodeData
        color = Just $ getColor config isActive isHovered visual
    drawEllipse renderer vp' color

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
