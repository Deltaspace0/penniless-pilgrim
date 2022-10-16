{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Parameters
    ( module ConfigSlider
    , module KeyConfig
    , AppParameters(..)
    , gridColumnsSlider
    , gridRowsSlider
    , linkToNodeSlider
    , nodeToWidthSlider
    , gameControlWidth
    , gameControlHeight
    , keyConfig
    , fromFile
    , fromJSON
    ) where

import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Default
import Data.Maybe
import System.IO

import ConfigSlider
import KeyConfig

data AppParameters = AppParameters
    { _apGridColumnsSlider :: ConfigSlider
    , _apGridRowsSlider    :: ConfigSlider
    , _apLinkToNodeSlider  :: ConfigSlider
    , _apNodeToWidthSlider :: ConfigSlider
    , _apGameControlWidth  :: Double
    , _apGameControlHeight :: Double
    , _apKeyConfig         :: KeyConfig
    } deriving (Eq, Show)

instance Default AppParameters where
    def = AppParameters
        { _apGridColumnsSlider = ConfigSlider 5 2 32 "Columns:"
        , _apGridRowsSlider    = ConfigSlider 5 2 32 "Rows:"
        , _apLinkToNodeSlider  = ConfigSlider 5 3 12
            "Link size to node size ratio:"
        , _apNodeToWidthSlider = ConfigSlider 3 2 12
            "Node size to link width ratio:"
        , _apGameControlWidth  = 400
        , _apGameControlHeight = 500
        , _apKeyConfig = def
        }

instance FromJSON AppParameters where
    parseJSON = withObject "AppParameters" $ \v -> AppParameters
        <$> v .: "grid_columns_slider"
        <*> v .: "grid_rows_slider"
        <*> v .: "link_to_node_slider"
        <*> v .: "node_to_width_slider"
        <*> v .: "game_control_width"
        <*> v .: "game_control_height"
        <*> v .: "key_config"

makeLensesWith abbreviatedFields 'AppParameters

fromFile :: String -> IO AppParameters
fromFile path = do
    file <- readFile path
    let contents = BLU.fromString file
        parameters = decode contents :: Maybe AppParameters
    if parameters == Nothing
        then return def
        else return $ fromJust parameters