module Composites.GameControl.GameControlModel
    ( GameControlModel(..)
    , initGameControlModel
    , getAnimationDuration
    , getLinkToNodeRatio
    , getNodeToWidthRatio
    , getDefaultNodeVisual
    ) where

import Data.Maybe
import Monomer

import Composites.GameControl.GameControlCfg
import Composites.GameControl.NodeVisual

data GameControlModel a = GameControlModel
    { _gcmControlledGame :: Maybe a
    , _gcmPreviewGame :: Maybe a
    , _gcmBackupGame :: Maybe a
    , _gcmReplaySequence :: [(Int, Int)]
    , _gcmShakeNode :: Maybe (Int, Int)
    , _gcmDuration :: Maybe Double
    , _gcmLinkNode :: Maybe Double
    , _gcmNodeWidth :: Maybe Double
    , _gcmVisual :: Maybe NodeVisual
    } deriving Eq

initGameControlModel :: GameControlCfg s e -> GameControlModel a
initGameControlModel config = GameControlModel
    { _gcmControlledGame = Nothing
    , _gcmPreviewGame = Nothing
    , _gcmBackupGame = Nothing
    , _gcmReplaySequence = []
    , _gcmShakeNode = Nothing
    , _gcmDuration = _gccDuration config
    , _gcmLinkNode = _gccLinkNode config
    , _gcmNodeWidth = _gccNodeWidth config
    , _gcmVisual = _gccVisual config
    }

getAnimationDuration :: GameControlModel a -> Double
getAnimationDuration model = fromMaybe 300 $ _gcmDuration model

getLinkToNodeRatio :: GameControlModel a -> Double
getLinkToNodeRatio model = fromMaybe 5 $ _gcmLinkNode model

getNodeToWidthRatio :: GameControlModel a -> Double
getNodeToWidthRatio model = fromMaybe 2 $ _gcmNodeWidth model

getDefaultNodeVisual :: GameControlModel a -> NodeVisual
getDefaultNodeVisual model = fromMaybe v $ _gcmVisual model where
    v = NodeVisual
        { _nodeColorHighlight = black
        , _nodeColorDefault = black
        , _nodeColorHover = black
        , _nodeColorActive = black
        }
