{-# LANGUAGE RecordWildCards #-}

module Composites.GameControl
    ( module Composites.GameControl.ControlledGame
    , module Composites.GameControl.GameControlCfg
    , module Composites.GameControl.LinkVisual
    , module Composites.GameControl.NodeVisual
    , GameControlEvent(EventSetReplay)
    , GameControlData(..)
    , gameControl
    ) where

import Control.Lens
import Data.Typeable
import Monomer

import Common.Grid
import Composites.GameControl.ControlledGame
import Composites.GameControl.GameControlCfg
import Composites.GameControl.GameControlEvent
import Composites.GameControl.GameControlModel
import Composites.GameControl.LinkVisual
import Composites.GameControl.NodeVisual
import Composites.GameControl.UI

data GameControlData s e a = GameControlData
    { _gcdGameLens :: ALens' s a
    , _gcdGetVisualGrid :: a -> Grid NodeVisual LinkVisual
    , _gcdConfig :: GameControlCfg s e
    }

gameControl
    :: (Typeable s, Typeable e, Typeable a, ControlledGame a)
    => GameControlData s e a
    -> WidgetNode s e
gameControl GameControlData{..} = node where
    node = compositeD_ wt wdata uiBuilder eventHandler cmpConfigs
    wt = "gameControl"
    wdata = WidgetValue $ initGameControlModel _gcdConfig
    uiBuilder = buildUI _gcdGetVisualGrid
    eventHandler = handleEvent _gcdConfig field
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
    getGame = flip widgetDataGet field
    field = WidgetLens _gcdGameLens
