module Composites.GameControl.GameControlModel
    ( GameControlModel(..)
    , initGameControlModel
    , getAnimationDuration
    , getReplayDuration
    , getLinkToNodeRatio
    , getNodeToWidthRatio
    , getDefaultNodeVisual
    ) where

import Data.Maybe
import Monomer

import Composites.GameControl.GameControlCfg
import Composites.GameControl.NodeVisual

data GameControlModel a b = GameControlModel
    { _gcmControlledGame :: Maybe a
    , _gcmPreviewGame :: Maybe a
    , _gcmBackupGame :: Maybe a
    , _gcmReplaySequence :: [(Int, Int)]
    , _gcmShakeNode :: Maybe (Int, Int)
    , _gcmDuration :: Maybe Double
    , _gcmReplayDuration :: Maybe Double
    , _gcmLinkNode :: Maybe Double
    , _gcmNodeWidth :: Maybe Double
    , _gcmVisual :: Maybe NodeVisual
    , _gcmColors :: Maybe b
    } deriving Eq

initGameControlModel :: GameControlCfg s e b -> GameControlModel a b
initGameControlModel config = GameControlModel
    { _gcmControlledGame = Nothing
    , _gcmPreviewGame = Nothing
    , _gcmBackupGame = Nothing
    , _gcmReplaySequence = []
    , _gcmShakeNode = Nothing
    , _gcmDuration = _gccDuration config
    , _gcmReplayDuration = _gccReplay config
    , _gcmLinkNode = _gccLinkNode config
    , _gcmNodeWidth = _gccNodeWidth config
    , _gcmVisual = _gccVisual config
    , _gcmColors = _gccColors config
    }

getAnimationDuration :: GameControlModel a b -> Double
getAnimationDuration model = fromMaybe 300 $ _gcmDuration model

getReplayDuration :: GameControlModel a b -> Double
getReplayDuration model = fromMaybe 100 $ _gcmReplayDuration model

getLinkToNodeRatio :: GameControlModel a b -> Double
getLinkToNodeRatio model = fromMaybe 5 $ _gcmLinkNode model

getNodeToWidthRatio :: GameControlModel a b -> Double
getNodeToWidthRatio model = fromMaybe 2 $ _gcmNodeWidth model

getDefaultNodeVisual :: GameControlModel a b -> NodeVisual
getDefaultNodeVisual model = fromMaybe v $ _gcmVisual model where
    v = NodeVisual
        { _nodeColorHighlight = black
        , _nodeColorDefault = black
        , _nodeColorHover = black
        , _nodeColorActive = black
        }
