{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Parameters.ColorConfig
    ( module Model.Parameters.LinkColorConfig
    , module Model.Parameters.NodeColorConfig
    , ColorConfig(..)
    ) where

import Data.Aeson
import Data.Default

import Model.Game
import Model.Parameters.LinkColorConfig
import Model.Parameters.NodeColorConfig
import Widgets.GameControl.GameControlColorConfig
import Widgets.GameControlLink
import Widgets.GameControlNode

data ColorConfig = ColorConfig
    { _ccGameControlLink :: LinkColorConfig
    , _ccGameControlNode :: NodeColorConfig
    } deriving (Eq, Show)

instance Default ColorConfig where
    def = ColorConfig def def

instance FromJSON ColorConfig where
    parseJSON = withObject "ColorConfig" $ \v -> ColorConfig
        <$> v .: "game_control_link"
        <*> v .: "game_control_node"

instance ToJSON ColorConfig where
    toJSON ColorConfig{..} = object
        [ "game_control_link" .= _ccGameControlLink
        , "game_control_node" .= _ccGameControlNode
        ]

instance GameControlColorConfig ColorConfig Game NodeColors where
    getDefaultNodeColors = _nccDefault . _ccGameControlNode
    getVisualGrid = transformGrid

transformGrid :: Game -> ColorConfig -> Grid NodeVisual LinkVisual
transformGrid Game{..} ColorConfig{..} = newGrid where
    newGrid = gridMap nt ht vt _grid 
    nt = nodeTransform _ccGameControlNode
    ht = hlinkTransform _ccGameControlLink
    vt = vlinkTransform _ccGameControlLink
