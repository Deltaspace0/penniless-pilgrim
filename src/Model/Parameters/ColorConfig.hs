{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Parameters.ColorConfig
    ( module Model.Parameters.LinkColorConfig
    , module Model.Parameters.NodeColorConfig
    , ColorConfig(..)
    , ccGameControlLink
    , ccGameControlNode
    , ccGameBackground
    , defaultVisual
    ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Default

import Common.Grid
import Model.Game
import Model.Parameters.LinkColorConfig
import Model.Parameters.NodeColorConfig
import Model.Parameters.RGB
import Composites.GameControl
import Monomer

data ColorConfig = ColorConfig
    { _ccGameControlLink :: LinkColorConfig
    , _ccGameControlNode :: NodeColorConfig
    , _ccGameBackground :: Color
    } deriving (Eq, Show)

makeLenses 'ColorConfig

instance Default ColorConfig where
    def = ColorConfig
        { _ccGameControlLink = def
        , _ccGameControlNode = def
        , _ccGameBackground = rgb 34 36 42
        }

instance FromJSON ColorConfig where
    parseJSON = withObject "ColorConfig" f where
        f v = ColorConfig
            <$> v .:? "game_control_link" .!= def
            <*> v .:? "game_control_node" .!= def
            <*> bg .!= _ccGameBackground def where
                bg = (fromRGB <$>) <$> v .:? "game_background"

instance ToJSON ColorConfig where
    toJSON ColorConfig{..} = object
        [ "game_control_link" .= _ccGameControlLink
        , "game_control_node" .= _ccGameControlNode
        , "game_background" .= toRGB _ccGameBackground
        ]

instance ControlledGameColors Game ColorConfig where
    getVisualGrid ColorConfig{..} Game{..} = newGrid where
        newGrid = gridMap nt ht vt _grid 
        nt = nodeTransform _ccGameControlNode
        ht = hlinkTransform _ccGameControlLink
        vt = vlinkTransform _ccGameControlLink
    getBackgroundColor = _ccGameBackground

defaultVisual :: ColorConfig -> NodeVisual
defaultVisual = colorsToVisual . _nccDefault . _ccGameControlNode
