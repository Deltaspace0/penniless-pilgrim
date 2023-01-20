{-# LANGUAGE OverloadedStrings #-}

module Model.Parameters.ColorConfig
    ( ColorConfig(..)
    ) where

import Data.Aeson
import Data.Default

import Widgets.GameControl.GameControlColorConfig
import Widgets.GameControlLink.LinkColorConfig
import Widgets.GameControlNode.NodeColorConfig

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

instance GameControlColorConfig ColorConfig where
    getLinkColorConfig = _ccGameControlLink
    getNodeColorConfig = _ccGameControlNode
