{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Model.Parameters
    ( module Model.Parameters.ConfigSlider
    , module Model.Parameters.Colors
    , AppParameters(..)
    , gridColumnsSlider
    , gridRowsSlider
    , linkToNodeSlider
    , nodeToWidthSlider
    , gameControlWidth
    , gameControlHeight
    , colors
    , fromFile
    , fromJSON
    ) where

import Control.Exception
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Default
import Data.Maybe
import System.IO

import Model.Parameters.ConfigSlider
import Model.Parameters.Colors

data AppParameters = AppParameters
    { _apGridColumnsSlider :: ConfigSlider
    , _apGridRowsSlider    :: ConfigSlider
    , _apLinkToNodeSlider  :: ConfigSlider
    , _apNodeToWidthSlider :: ConfigSlider
    , _apGameControlWidth  :: Double
    , _apGameControlHeight :: Double
    , _apColors :: Colors
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
        , _apColors = def
        }

instance FromJSON AppParameters where
    parseJSON = withObject "AppParameters" $ \v -> AppParameters
        <$> v .: "grid_columns_slider"
        <*> v .: "grid_rows_slider"
        <*> v .: "link_to_node_slider"
        <*> v .: "node_to_width_slider"
        <*> v .: "game_control_width"
        <*> v .: "game_control_height"
        <*> v .: "colors"

makeLensesWith abbreviatedFields 'AppParameters

fromFile :: String -> IO AppParameters
fromFile path = do
    let handler = const $ return "" :: SomeException -> IO String
    file <- catch (readFile path) handler
    let contents = BLU.fromString file
        parameters = decode contents :: Maybe AppParameters
    return $ fromMaybe def parameters