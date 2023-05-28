{-# LANGUAGE RecordWildCards #-}

module Widgets.GameControl
    ( module Widgets.GameControl.ControlledGame
    , module Widgets.GameControl.GameControlCfg
    , module Widgets.GameControl.LinkVisual
    , module Widgets.GameControl.NodeVisual
    , GameControlData(..)
    , gameControl
    ) where

import Control.Lens
import Data.Typeable
import Monomer

import Common.Grid
import Widgets.GameControl.ControlledGame
import Widgets.GameControl.GameControlCfg
import Widgets.GameControl.GameControlEvent
import Widgets.GameControl.GameControlModel
import Widgets.GameControl.LinkVisual
import Widgets.GameControl.NodeVisual
import Widgets.GameControl.UI

data GameControlData s a = GameControlData
    { _gcdGameLens :: ALens' s a
    , _gcdGetVisualGrid :: a -> Grid NodeVisual LinkVisual
    , _gcdConfig :: GameControlCfg s
    }

gameControl
    :: (Typeable s, Typeable e, Typeable a, ControlledGame a)
    => GameControlData s a
    -> WidgetNode s e
gameControl GameControlData{..} = node where
    node = compositeD_ wt wdata uiBuilder eventHandler cmpConfigs
    wt = "gameControl"
    wdata = WidgetValue initGameControlModel
    uiBuilder = buildUI _gcdGetVisualGrid _gcdConfig
    eventHandler = handleEvent _gcdConfig field
    cmpConfigs =
        [ mergeRequired $ \_ _ _ -> True
        , compositeMergeEvents $ \_ _ _ _ _ _ -> [EventStopShake]
        , compositeMergeModel mergeHandler
        ]
    mergeHandler _ parentModel oldModel _ = oldModel
        { _gcmControlledGame = Just $ getGame parentModel
        }
    getGame = flip widgetDataGet field
    field = WidgetLens _gcdGameLens
