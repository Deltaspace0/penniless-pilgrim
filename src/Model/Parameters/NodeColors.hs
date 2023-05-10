{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Parameters.NodeColors
    ( NodeColors(..)
    , colorsToVisual
    , nodeHighlight
    , nodeDefault
    , nodeHover
    , nodeActive
    ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Monomer

import Model.Parameters.RGB
import Widgets.GameControlNode

data NodeColors = NodeColors
    { _nodeHighlight :: Color
    , _nodeDefault :: Color
    , _nodeHover :: Color
    , _nodeActive :: Color
    } deriving (Eq, Show)

makeLenses 'NodeColors

instance FromJSON NodeColors where
    parseJSON = withObject "NodeColors" f where
        f v = NodeColors
            <$> g "highlight"
            <*> g "default"
            <*> g "hover"
            <*> g "active" where
                g t = fromRGB <$> v .: t

instance ToJSON NodeColors where
    toJSON NodeColors{..} = object
        [ "highlight" .= toRGB _nodeHighlight
        , "default" .= toRGB _nodeDefault
        , "hover" .= toRGB _nodeHover
        , "active" .= toRGB _nodeActive
        ]

colorsToVisual :: NodeColors -> NodeVisual
colorsToVisual NodeColors{..} = NodeVisual
    { _nodeColorHighlight = _nodeHighlight
    , _nodeColorDefault = _nodeDefault
    , _nodeColorHover = _nodeHover
    , _nodeColorActive = _nodeActive
    }
