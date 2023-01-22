module Widgets.GameControl.GameControlData
    ( GameControlData(..)
    , getFixedRect
    , getMiddleRect
    , assignAreas
    , makeChildren
    , handleGameChange
    ) where

import Control.Lens
import Data.Maybe
import Data.Sequence (Seq, (><))
import Monomer
import qualified Data.Sequence as Seq
import qualified Monomer.Lens as L

import Model.Grid
import Widgets.GameControl.ControlledGame
import Widgets.GameControl.GameControlConfig
import Widgets.GameControlLink
import Widgets.GameControlNode

data GameControlData s a b = GameControlData
    { _gcdGameLens :: ALens' s a
    , _gcdNextTaxLens :: ALens' s (Maybe Double)
    , _gcdConfig :: b
    }

getFixedRect
    :: (ControlledGame a, GameControlConfig b c)
    => GameControlData s a b
    -> WidgetEnv s e
    -> Rect
getFixedRect gcData wenv = Rect x y linkSize nodeSize where
    x = (width'-linkSize*factorW)/2
    y = (height'-linkSize*factorH)/2
    linkSize = min (width'/factorW) (height'/factorH)
    nodeSize = linkSize/linkToNodeRatio
    factorW = (fromIntegral cols)+2/linkToNodeRatio
    factorH = (fromIntegral rows)+2/linkToNodeRatio
    (cols, rows) = getBounds $ getGrid gcData wenv
    width' = getWidth config
    height' = getHeight config
    linkToNodeRatio = getLinkToNodeRatio config
    config = _gcdConfig gcData

getMiddleRect :: Rect -> Rect -> Double -> Rect
getMiddleRect rectA rectB progress = rect where
    rect = Rect x y linkSize nodeSize
    x = xA+(xB-xA)*progress
    y = yA+(yB-yA)*progress
    linkSize = linkSizeA+(linkSizeB-linkSizeA)*progress
    nodeSize = nodeSizeA+(nodeSizeB-nodeSizeA)*progress
    Rect xA yA linkSizeA nodeSizeA = rectA
    Rect xB yB linkSizeB nodeSizeB = rectB

assignAreas
    :: (ControlledGame a, GameControlConfig b c)
    => WidgetEnv s e
    -> Rect
    -> GameControlData s a b
    -> Seq Rect
assignAreas wenv vp gcData = assignedAreas where
    assignedAreas =
           fmap (getNodeArea . fst) (getNodeSequence grid)
        >< fmap (getLinkArea . fst) (getHlinkSequence grid)
        >< fmap (getLinkArea . fst) (getVlinkSequence grid)
    grid = getGrid gcData wenv
    getNodeArea (i, j) = Rect x' y' d d where
        x' = vx+linkSize*(fromIntegral i)
        y' = vy+linkSize*(fromIntegral j)
        d = nodeSize*2
    getLinkArea (i, j) = Rect x' y' linkSize nodeSize where
        x' = vx+linkSize*(fromIntegral i)+nodeSize
        y' = vy+linkSize*(fromIntegral j)+nodeSize
    vx = x+(vp ^. L.x)
    vy = y+(vp ^. L.y)
    Rect x y linkSize nodeSize = getFixedRect gcData wenv

makeChildren
    :: (ControlledGame a, GameControlConfig b c)
    => WidgetEnv s e
    -> WidgetNode s e
    -> GameControlData s a b
    -> WidgetNode s e
makeChildren wenv node gcData = resNode where
    resNode = node & L.children .~ fmap fn nodeSequence
        >< fmap fh hlinkSequence
        >< fmap fv vlinkSequence
    game = widgetDataGet (wenv ^. L.model) gameLens
    grid = gridFromGame game config
    nodeSequence = getNodeSequence grid
    hlinkSequence = getHlinkSequence grid
    vlinkSequence = getVlinkSequence grid
    fn (p, visualStack) = gameControlNode $ NodeData
        { _ndVisualStack = visualStack
        , _ndGameControlId = widgetId
        , _ndPosition = p
        , _ndClickable = not $ null tax
        , _ndNextTax = tax
        , _ndNextTaxLens = nextTaxLens
        , _ndColorConfig = nodeColorConfig
        , _ndAnimationDuration = getAnimationDuration config
        } where tax = getScoreByPosition p game
    fh = gameControlHlink . linkData
    fv = gameControlVlink . linkData
    linkData (p, link) = LinkData
        { _ldLink = link
        , _ldPosition = p
        , _ldAnimationDuration = getAnimationDuration config
        , _ldNodeToWidthRatio = getNodeToWidthRatio config
        }
    widgetId = node ^. L.info . L.widgetId
    config = _gcdConfig gcData
    gameLens = WidgetLens $ _gcdGameLens gcData
    nextTaxLens = WidgetLens $ _gcdNextTaxLens gcData
    nodeColorConfig = getNodeColorConfig $ getColorConfig config

handleGameChange
    :: (ControlledGame a, GameControlConfig b c)
    => WidgetEnv s e
    -> WidgetNode s e
    -> GameControlData s a b
    -> (a -> Maybe a)
    -> Maybe (WidgetResult s e)
handleGameChange wenv node gcData f = result where
    result = Just $ resultReqs node reqs
    nextGame = f game
    nextGame' = fromJust nextGame
    game = widgetDataGet (wenv ^. L.model) gameLens
    reqs = if null nextGame
        then [SendMessage shakeNodeId NodeStartShake]
        else concat
            [ widgetDataSet gameLens nextGame'
            , widgetDataSet nextTaxLens Nothing
            ]
    shakeNodeId = shakeNode ^. L.info ^. L.widgetId
    shakeNode = Seq.index (node ^. L.children) $ x*(rows+1)+y
    (x, y) = getCurrentPosition game
    rows = snd $ getBounds $ getGrid gcData wenv
    gameLens = WidgetLens $ _gcdGameLens gcData
    nextTaxLens = WidgetLens $ _gcdNextTaxLens gcData

gridFromGame
    :: (ControlledGame a, GameControlConfig b c)
    => a
    -> b
    -> Grid NodeVisual LinkVisual
gridFromGame game config = gridMap nt ht vt $ getGameGrid game where
    nt = nodeTransform $ getNodeColorConfig $ getColorConfig config
    ht = hlinkTransform linkColorConfig
    vt = vlinkTransform linkColorConfig
    linkColorConfig = getLinkColorConfig $ getColorConfig config

getGrid
    :: (ControlledGame a, GameControlConfig b c)
    => GameControlData s a b
    -> WidgetEnv s e
    -> Grid NodeVisual LinkVisual
getGrid gcData wenv = gridFromGame game config where
    game = widgetDataGet (wenv ^. L.model) gameLens
    config = _gcdConfig gcData
    gameLens = WidgetLens $ _gcdGameLens gcData
