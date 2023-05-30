{-# LANGUAGE RecordWildCards #-}

module Composites.GameControl
    ( module Composites.GameControl.ControlledGame
    , module Composites.GameControl.ControlledGameColors
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

import Composites.GameControl.ControlledGame
import Composites.GameControl.ControlledGameColors
import Composites.GameControl.GameControlCfg
import Composites.GameControl.GameControlEvent
import Composites.GameControl.GameControlModel
import Composites.GameControl.LinkVisual
import Composites.GameControl.NodeVisual
import Composites.GameControl.UI

data GameControlData s e a b = GameControlData
    { _gcdGameLens :: ALens' s a
    , _gcdConfig :: GameControlCfg s e b
    }

gameControl
    :: ( Typeable s
       , Typeable e
       , Typeable a
       , Typeable b
       , ControlledGame a
       , ControlledGameColors a b
       )
    => GameControlData s e a b
    -> WidgetNode s e
gameControl GameControlData{..} = node where
    node = compositeD_ wt wdata buildUI eventHandler cmpConfigs
    wt = "gameControl"
    wdata = WidgetValue $ initGameControlModel _gcdConfig
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
