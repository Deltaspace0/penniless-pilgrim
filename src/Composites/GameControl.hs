{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Composites.GameControl
    ( module Composites.GameControl.ControlledGame
    , module Composites.GameControl.ControlledGameColors
    , module Composites.GameControl.GameControlCfg
    , module Composites.GameControl.LinkVisual
    , module Composites.GameControl.NodeVisual
    , GameControlEvent(EventSetReplay)
    , gameControl
    , gameControl_
    ) where

import Control.Lens
import Data.Typeable
import Monomer

import Composites.GameControl.ControlledGame
import Composites.GameControl.ControlledGameColors
import Composites.GameControl.GameControlCfg
import Composites.GameControl.GameControlEvent
import Composites.GameControl.GameControlModel
import Composites.GameControl.LinkVisual
import Composites.GameControl.NodeVisual
import Composites.GameControl.UI

gameControl
    :: forall s e a b .
        (Typeable s, Typeable e, ControlledGameColors a b)
    => ALens' s a
    -> WidgetNode s e
gameControl field = gameControl_ field configs where
    configs = [] :: [GameControlCfg s e b]

gameControl_
    :: (Typeable s, Typeable e, ControlledGameColors a b)
    => ALens' s a
    -> [GameControlCfg s e b]
    -> WidgetNode s e
gameControl_ field configs = node where
    node = compositeD_ wt wdata buildUI eventHandler cmpConfigs
    wt = "gameControl"
    wdata = WidgetValue $ initGameControlModel config
    eventHandler = handleEvent config wlens
    config = mconcat configs
    cmpConfigs =
        [ compositeMergeEvents $ \_ _ _ _ _ _ -> [EventStopShake]
        , compositeMergeModel mergeHandler
        ]
    mergeHandler _ parentModel oldModel newModel = model where
        model = newModel
            { _gcmControlledGame = if sameGame
                then _gcmControlledGame oldModel
                else game
            , _gcmPreviewGame = if sameGame
                then _gcmPreviewGame oldModel
                else Nothing
            , _gcmBackupGame = game
            , _gcmReplaySequence = if sameGame
                then _gcmReplaySequence oldModel
                else []
            , _gcmShakeNode = _gcmShakeNode oldModel
            }
        sameGame = game == _gcmBackupGame oldModel
        game = Just $ getGame parentModel
    getGame = flip widgetDataGet wlens
    wlens = WidgetLens field
