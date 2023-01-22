{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Parameters.ColorConfig
    ( ColorConfig(..)
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
    toJSON config = object
        [ "game_control_link" .= _ccGameControlLink config
        , "game_control_node" .= _ccGameControlNode config
        ]

instance GameControlColorConfig ColorConfig Game where
    getDefaultNodeColors = _nccDefault . _ccGameControlNode
    getVisualGrid = transformGrid

transformGrid :: Game -> ColorConfig -> Grid NodeVisual LinkVisual
transformGrid game config = gridMap nt ht vt $ _grid game where
    nt = nodeTransform $ _ccGameControlNode config
    ht = hlinkTransform $ _ccGameControlLink config
    vt = vlinkTransform $ _ccGameControlLink config
