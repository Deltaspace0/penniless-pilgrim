{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControlLink.LinkColorConfig
    ( LinkColorConfig(..)
    ) where

import Data.Aeson
import Data.Default
import Monomer

import Model.Parameters.RGB

data LinkColorConfig = LinkColorConfig
    { _lccDefault :: Color
    , _lccNorth :: Color
    , _lccSouth :: Color
    , _lccWest :: Color
    , _lccEast :: Color
    } deriving (Eq, Show)

instance Default LinkColorConfig where
    def = LinkColorConfig
        { _lccDefault = rgb 120 120 117
        , _lccNorth = rgb 247 105 70
        , _lccSouth = rgb 247 105 70
        , _lccWest = rgb 247 105 70
        , _lccEast = rgb 247 105 70
        }

instance FromJSON LinkColorConfig where
    parseJSON = withObject "LinkColorConfig" f where
        f v = LinkColorConfig
            <$> g "default"
            <*> g "north"
            <*> g "south"
            <*> g "west"
            <*> g "east" where
                g t = fromRGB <$> v .: t

instance ToJSON LinkColorConfig where
    toJSON config = object
        [ "default" .= toRGB (_lccDefault config)
        , "north" .= toRGB (_lccNorth config)
        , "south" .= toRGB (_lccSouth config)
        , "west" .= toRGB (_lccWest config)
        , "east" .= toRGB (_lccEast config)
        ]
