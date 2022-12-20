{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControl.GameControlColorConfig
    ( module Widgets.GameControl.GameControlLink.LinkColorConfig
    , module Widgets.GameControl.GameControlNode.NodeColorConfig
    , GameControlColorConfig(..)
    ) where

import Data.Aeson
import Data.Default

import Widgets.GameControl.GameControlLink.LinkColorConfig
import Widgets.GameControl.GameControlNode.NodeColorConfig

data GameControlColorConfig = GameControlColorConfig
    { _gcccLink :: LinkColorConfig
    , _gcccNode :: NodeColorConfig
    } deriving (Eq, Show)

instance Default GameControlColorConfig where
    def = GameControlColorConfig def def

instance FromJSON GameControlColorConfig where
    parseJSON = withObject "GameControlColorConfig" f where
        f v = GameControlColorConfig <$> v .: "link" <*> v .: "node"

instance ToJSON GameControlColorConfig where
    toJSON config = object
        [ "link" .= _gcccLink config
        , "node" .= _gcccNode config
        ]