{-# LANGUAGE FunctionalDependencies #-}

module Widgets.GameControl.GameControlCfg
    ( GameControlCfg(..)
    , gameLinkToNodeRatio
    , gameNodeToWidthRatio
    , gameDefaultNodeVisual
    , gameNextScoreField
    , getAnimationDuration
    , getLinkToNodeRatio
    , getNodeToWidthRatio
    , getDefaultNodeVisual
    ) where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Default
import Data.Maybe
import Monomer

import Widgets.GameControl.NodeVisual

data GameControlCfg s = GameControlCfg
    { _gccDuration :: Maybe Double
    , _gccLinkNode :: Maybe Double
    , _gccNodeWidth :: Maybe Double
    , _gccVisual :: Maybe NodeVisual
    , _gccNextScore :: Maybe (ALens' s (Maybe Double))
    }

instance Default (GameControlCfg s) where
    def = GameControlCfg
        { _gccDuration = Nothing
        , _gccLinkNode = Nothing
        , _gccNodeWidth = Nothing
        , _gccVisual = Nothing
        , _gccNextScore = Nothing
        }

instance Semigroup (GameControlCfg s) where
    (<>) gc1 gc2 = GameControlCfg
        { _gccDuration = _gccDuration gc2 <|> _gccDuration gc1
        , _gccLinkNode = _gccLinkNode gc2 <|> _gccLinkNode gc1
        , _gccNodeWidth = _gccNodeWidth gc2 <|> _gccNodeWidth gc1
        , _gccVisual = _gccVisual gc2 <|> _gccVisual gc1
        , _gccNextScore = _gccNextScore gc2 <|> _gccNextScore gc1
        }

instance Monoid (GameControlCfg s) where
    mempty = def

instance CmbDuration (GameControlCfg e) Double where
    duration dur = def {
        _gccDuration = Just dur
    }

gameLinkToNodeRatio :: Double -> GameControlCfg s
gameLinkToNodeRatio v = def
    { _gccLinkNode = Just v
    }

gameNodeToWidthRatio :: Double -> GameControlCfg s
gameNodeToWidthRatio v = def
    { _gccNodeWidth = Just v
    }

gameDefaultNodeVisual :: NodeVisual -> GameControlCfg s
gameDefaultNodeVisual v = def
    { _gccVisual = Just v
    }

gameNextScoreField :: ALens' s (Maybe Double) -> GameControlCfg s
gameNextScoreField v = def
    { _gccNextScore = Just v
    }

getAnimationDuration :: GameControlCfg s -> Double
getAnimationDuration config = fromMaybe 300 $ _gccDuration config

getLinkToNodeRatio :: GameControlCfg s -> Double
getLinkToNodeRatio config = fromMaybe 5 $ _gccLinkNode config

getNodeToWidthRatio :: GameControlCfg s -> Double
getNodeToWidthRatio config = fromMaybe 2 $ _gccNodeWidth config

getDefaultNodeVisual :: GameControlCfg s -> NodeVisual
getDefaultNodeVisual config = fromMaybe v $ _gccVisual config where
    v = NodeVisual
        { _nodeColorHighlight = black
        , _nodeColorDefault = black
        , _nodeColorHover = black
        , _nodeColorActive = black
        }
