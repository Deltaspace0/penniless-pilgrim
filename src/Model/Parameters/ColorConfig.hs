{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Parameters.ColorConfig
    ( module Model.Parameters.LinkColorConfig
    , module Model.Parameters.NodeColorConfig
    , ColorConfig(..)
    , ccGameControlLink
    , ccGameControlNode
    , defaultVisual
    , transformGrid
    ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Default

import Common.Grid
import Model.Game
import Model.Parameters.LinkColorConfig
import Model.Parameters.NodeColorConfig
import Widgets.GameControl

data ColorConfig = ColorConfig
    { _ccGameControlLink :: LinkColorConfig
    , _ccGameControlNode :: NodeColorConfig
    } deriving (Eq, Show)

makeLenses 'ColorConfig

instance Default ColorConfig where
    def = ColorConfig def def

instance FromJSON ColorConfig where
    parseJSON = withObject "ColorConfig" $ \v -> ColorConfig
        <$> v .:? "game_control_link" .!= def
        <*> v .:? "game_control_node" .!= def

instance ToJSON ColorConfig where
    toJSON ColorConfig{..} = object
        [ "game_control_link" .= _ccGameControlLink
        , "game_control_node" .= _ccGameControlNode
        ]

defaultVisual :: ColorConfig -> NodeVisual
defaultVisual = colorsToVisual . _nccDefault . _ccGameControlNode

transformGrid :: Game -> ColorConfig -> Grid NodeVisual LinkVisual
transformGrid Game{..} ColorConfig{..} = newGrid where
    newGrid = gridMap nt ht vt _grid 
    nt = nodeTransform _ccGameControlNode
    ht = hlinkTransform _ccGameControlLink
    vt = vlinkTransform _ccGameControlLink
