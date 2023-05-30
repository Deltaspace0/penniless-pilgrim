{-# LANGUAGE FunctionalDependencies #-}

module Composites.GameControl.GameControlCfg
    ( GameControlCfg(..)
    , gameLinkToNodeRatio
    , gameNodeToWidthRatio
    , gameDefaultNodeVisual
    , gameColors
    , gameNextScoreField
    , gameReplayStepDuration
    , gameOnReplayed
    ) where

import Control.Applicative ((<|>))
import Control.Lens
import Data.Default
import Monomer

import Composites.GameControl.NodeVisual

data GameControlCfg s e a = GameControlCfg
    { _gccDuration :: Maybe Double
    , _gccLinkNode :: Maybe Double
    , _gccNodeWidth :: Maybe Double
    , _gccVisual :: Maybe NodeVisual
    , _gccColors :: Maybe a
    , _gccNextScore :: Maybe (ALens' s (Maybe Double))
    , _gccReplay :: Maybe Double
    , _gccOnReplayed :: [e]
    }

instance Default (GameControlCfg s e a) where
    def = GameControlCfg
        { _gccDuration = Nothing
        , _gccLinkNode = Nothing
        , _gccNodeWidth = Nothing
        , _gccVisual = Nothing
        , _gccColors = Nothing
        , _gccNextScore = Nothing
        , _gccReplay = Nothing
        , _gccOnReplayed = []
        }

instance Semigroup (GameControlCfg s e a) where
    (<>) gc1 gc2 = GameControlCfg
        { _gccDuration = _gccDuration gc2 <|> _gccDuration gc1
        , _gccLinkNode = _gccLinkNode gc2 <|> _gccLinkNode gc1
        , _gccNodeWidth = _gccNodeWidth gc2 <|> _gccNodeWidth gc1
        , _gccVisual = _gccVisual gc2 <|> _gccVisual gc1
        , _gccColors = _gccColors gc2 <|> _gccColors gc1
        , _gccNextScore = _gccNextScore gc2 <|> _gccNextScore gc1
        , _gccReplay = _gccReplay gc2 <|> _gccReplay gc1
        , _gccOnReplayed = _gccOnReplayed gc1 <> _gccOnReplayed gc2
        }

instance Monoid (GameControlCfg s e a) where
    mempty = def

instance CmbDuration (GameControlCfg s e a) Double where
    duration dur = def {
        _gccDuration = Just dur
    }

gameLinkToNodeRatio :: Double -> GameControlCfg s e a
gameLinkToNodeRatio v = def
    { _gccLinkNode = Just v
    }

gameNodeToWidthRatio :: Double -> GameControlCfg s e a
gameNodeToWidthRatio v = def
    { _gccNodeWidth = Just v
    }

gameDefaultNodeVisual :: NodeVisual -> GameControlCfg s e a
gameDefaultNodeVisual v = def
    { _gccVisual = Just v
    }

gameColors :: a -> GameControlCfg s e a
gameColors v = def
    { _gccColors = Just v
    }

gameNextScoreField
    :: ALens' s (Maybe Double)
    -> GameControlCfg s e a
gameNextScoreField v = def
    { _gccNextScore = Just v
    }

gameReplayStepDuration :: Double -> GameControlCfg s e a
gameReplayStepDuration dur = def
    { _gccReplay = Just dur
    }

gameOnReplayed :: e -> GameControlCfg s e a
gameOnReplayed event = def
    { _gccOnReplayed = [event]
    }
