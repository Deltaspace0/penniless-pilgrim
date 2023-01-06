{-# LANGUAGE OverloadedStrings #-}

module Composites.Config.Parameters.ColorConfig
    ( ColorConfig(..)
    ) where

import Data.Aeson
import Data.Default

import Widgets.GameControl.GameControlColorConfig

data ColorConfig = ColorConfig
    { _ccGameControl :: GameControlColorConfig
    } deriving (Eq, Show)

instance Default ColorConfig where
    def = ColorConfig def

instance FromJSON ColorConfig where
    parseJSON = withObject "ColorConfig" $ \v -> ColorConfig
        <$> v .: "game_control"

instance ToJSON ColorConfig where
    toJSON config = object
        [ "game_control" .= _ccGameControl config
        ]
