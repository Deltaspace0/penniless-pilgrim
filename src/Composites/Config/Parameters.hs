{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Composites.Config.Parameters
    ( module Composites.Config.Parameters.ColorConfig
    , module Composites.Config.Parameters.ConfigSlider
    , Parameters(..)
    , gridColumnsSlider
    , gridRowsSlider
    , gridAnimationSlider
    , linkToNodeSlider
    , nodeToWidthSlider
    , gameControlWidth
    , gameControlHeight
    , colorConfig
    , gameFromParameters
    , parametersFromFile
    , parametersToFile
    ) where

import Control.Exception
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Default
import Data.Maybe
import System.IO
import qualified Data.ByteString.Lazy.UTF8 as BLU

import Composites.Config.Parameters.ColorConfig
import Composites.Config.Parameters.ConfigSlider
import Model.Game

data Parameters = Parameters
    { _apGridColumnsSlider :: ConfigSlider
    , _apGridRowsSlider :: ConfigSlider
    , _apGridAnimationSlider :: ConfigSlider
    , _apLinkToNodeSlider :: ConfigSlider
    , _apNodeToWidthSlider :: ConfigSlider
    , _apGameControlWidth :: Double
    , _apGameControlHeight :: Double
    , _apColorConfig :: ColorConfig
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'Parameters

instance Default Parameters where
    def = Parameters
        { _apGridColumnsSlider = ConfigSlider
            { _csCurrent = 5
            , _csMin = 2
            , _csMax = 32
            , _csChangeRate = 1
            , _csCaption = "Columns:"
            }
        , _apGridRowsSlider = ConfigSlider
            { _csCurrent = 5
            , _csMin = 2
            , _csMax = 32
            , _csChangeRate = 1
            , _csCaption = "Rows:"
            }
        , _apGridAnimationSlider = ConfigSlider
            { _csCurrent = 300
            , _csMin = 0
            , _csMax = 1000
            , _csChangeRate = 10
            , _csCaption = "Animation duration (in milliseconds):"
            }
        , _apLinkToNodeSlider = ConfigSlider
            { _csCurrent = 5
            , _csMin = 3
            , _csMax = 8
            , _csChangeRate = 0.2
            , _csCaption = "Link size to node size ratio:"
            }
        , _apNodeToWidthSlider = ConfigSlider
            { _csCurrent = 2
            , _csMin = 1
            , _csMax = 8
            , _csChangeRate = 0.2
            , _csCaption = "Node size to link width ratio:"
            }
        , _apGameControlWidth = 400
        , _apGameControlHeight = 500
        , _apColorConfig = def
        }

instance FromJSON Parameters where
    parseJSON = withObject "Parameters" $ \v -> Parameters
        <$> v .: "grid_columns_slider"
        <*> v .: "grid_rows_slider"
        <*> v .: "grid_animation_slider"
        <*> v .: "link_to_node_slider"
        <*> v .: "node_to_width_slider"
        <*> v .: "game_control_width"
        <*> v .: "game_control_height"
        <*> v .: "color_config"

instance ToJSON Parameters where
    toJSON p = object
        [ "grid_columns_slider" .= (p ^. gridColumnsSlider)
        , "grid_rows_slider" .= (p ^. gridRowsSlider)
        , "grid_animation_slider" .= (p ^. gridAnimationSlider)
        , "link_to_node_slider" .= (p ^. linkToNodeSlider)
        , "node_to_width_slider" .= (p ^. nodeToWidthSlider)
        , "game_control_width" .= (p ^. gameControlWidth)
        , "game_control_height" .= (p ^. gameControlHeight)
        , "color_config" .= (p ^. colorConfig)
        ]

gameFromParameters :: Parameters -> Game
gameFromParameters parameters' = game where
    game = makeGame (floor gridColumns) (floor gridRows)
    gridColumns = parameters' ^. gridColumnsSlider . csCurrent
    gridRows = parameters' ^. gridRowsSlider . csCurrent

parametersFromFile :: String -> IO (Bool, Parameters)
parametersFromFile path = do
    let handler = const $ return "" :: SomeException -> IO String
    file <- catch (readFile path) handler
    let contents = BLU.fromString file
        parameters = decode contents :: Maybe Parameters
    return $ fromMaybe (False, def) $ (,) True <$> parameters

parametersToFile :: Parameters -> String -> IO Bool
parametersToFile parameters path = do
    let config = defConfig {confCompare = flip compare}
        converted = encodePretty' config parameters
        contents = BLU.toString converted
        operation = writeFile path contents
    result <- try operation :: IO (Either SomeException ())
    case result of
        Left _ -> return False
        Right _ -> return True
