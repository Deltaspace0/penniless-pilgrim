{-# LANGUAGE RecordWildCards #-}

module Model.Parameters.NodeColors
    ( NodeColors(..)
    ) where

import Data.Aeson
import Monomer

import Model.Parameters.RGB
import Widgets.ButtonColors

data NodeColors = NodeColors
    { _nodeHighlight :: Color
    , _nodeDefault :: Color
    , _nodeHover :: Color
    , _nodeActive :: Color
    } deriving (Eq, Show)

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

instance ButtonColors NodeColors where
    getDefaultColor = _nodeDefault
    getHighlightColor = _nodeHighlight
    getHoverColor = _nodeHover
    getActiveColor = _nodeActive
