{-# LANGUAGE FunctionalDependencies #-}

module Composites.GameControl.GameControlCfg
    ( GameControlCfg(..)
    , gameLinkToNodeRatio
    , gameNodeToWidthRatio
    , gameDefaultNodeVisual
    , gameNextScoreField
    , gameOnReplayed
    ) where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Default
import Monomer

import Composites.GameControl.NodeVisual

data GameControlCfg s e = GameControlCfg
    { _gccDuration :: Maybe Double
    , _gccLinkNode :: Maybe Double
    , _gccNodeWidth :: Maybe Double
    , _gccVisual :: Maybe NodeVisual
    , _gccNextScore :: Maybe (ALens' s (Maybe Double))
    , _gccOnReplayed :: [e]
    }

instance Default (GameControlCfg s e) where
    def = GameControlCfg
        { _gccDuration = Nothing
        , _gccLinkNode = Nothing
        , _gccNodeWidth = Nothing
        , _gccVisual = Nothing
        , _gccNextScore = Nothing
        , _gccOnReplayed = []
        }

instance Semigroup (GameControlCfg s e) where
    (<>) gc1 gc2 = GameControlCfg
        { _gccDuration = _gccDuration gc2 <|> _gccDuration gc1
        , _gccLinkNode = _gccLinkNode gc2 <|> _gccLinkNode gc1
        , _gccNodeWidth = _gccNodeWidth gc2 <|> _gccNodeWidth gc1
        , _gccVisual = _gccVisual gc2 <|> _gccVisual gc1
        , _gccNextScore = _gccNextScore gc2 <|> _gccNextScore gc1
        , _gccOnReplayed = _gccOnReplayed gc1 <> _gccOnReplayed gc2
        }

instance Monoid (GameControlCfg s e) where
    mempty = def

instance CmbDuration (GameControlCfg s e) Double where
    duration dur = def {
        _gccDuration = Just dur
    }

gameLinkToNodeRatio :: Double -> GameControlCfg s e
gameLinkToNodeRatio v = def
    { _gccLinkNode = Just v
    }

gameNodeToWidthRatio :: Double -> GameControlCfg s e
gameNodeToWidthRatio v = def
    { _gccNodeWidth = Just v
    }

gameDefaultNodeVisual :: NodeVisual -> GameControlCfg s e
gameDefaultNodeVisual v = def
    { _gccVisual = Just v
    }

gameNextScoreField :: ALens' s (Maybe Double) -> GameControlCfg s e
gameNextScoreField v = def
    { _gccNextScore = Just v
    }

gameOnReplayed :: e -> GameControlCfg s e
gameOnReplayed event = def
    { _gccOnReplayed = [event]
    }
