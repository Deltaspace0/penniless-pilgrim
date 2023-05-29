{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Parameters
    ( module Model.Parameters.ColorConfig
    , module Model.Parameters.ConfigSlider
    , Parameters(..)
    , gridColumnsSlider
    , gridRowsSlider
    , gridAnimationSlider
    , gridReplaySlider
    , linkToNodeSlider
    , nodeToWidthSlider
    , pilgrimStartXSlider
    , pilgrimStartYSlider
    , colorConfig
    , gameFromParameters
    , parametersFromGame
    ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Default

import Common
import Model.Parameters.ColorConfig
import Model.Parameters.ConfigSlider
import Model.Game

data Parameters = Parameters
    { _apGridColumnsSlider :: ConfigSlider
    , _apGridRowsSlider :: ConfigSlider
    , _apGridAnimationSlider :: ConfigSlider
    , _apGridReplaySlider :: ConfigSlider
    , _apLinkToNodeSlider :: ConfigSlider
    , _apNodeToWidthSlider :: ConfigSlider
    , _apPilgrimStartXSlider :: ConfigSlider
    , _apPilgrimStartYSlider :: ConfigSlider
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
            , _csCaption = "Columns"
            }
        , _apGridRowsSlider = ConfigSlider
            { _csCurrent = 5
            , _csMin = 2
            , _csMax = 32
            , _csChangeRate = 1
            , _csCaption = "Rows"
            }
        , _apGridAnimationSlider = ConfigSlider
            { _csCurrent = 300
            , _csMin = 0
            , _csMax = 1000
            , _csChangeRate = 10
            , _csCaption = "Animation duration (in milliseconds)"
            }
        , _apGridReplaySlider = ConfigSlider
            { _csCurrent = 100
            , _csMin = 10
            , _csMax = 1000
            , _csChangeRate = 10
            , _csCaption = "Replay step duration (in milliseconds)"
            }
        , _apLinkToNodeSlider = ConfigSlider
            { _csCurrent = 5
            , _csMin = 3
            , _csMax = 8
            , _csChangeRate = 0.2
            , _csCaption = "Link size to node size ratio"
            }
        , _apNodeToWidthSlider = ConfigSlider
            { _csCurrent = 2
            , _csMin = 1
            , _csMax = 8
            , _csChangeRate = 0.2
            , _csCaption = "Node size to link width ratio"
            }
        , _apPilgrimStartXSlider = ConfigSlider
            { _csCurrent = 0
            , _csMin = 0
            , _csMax = 32
            , _csChangeRate = 1
            , _csCaption = "Pilgrim start position X"
            }
        , _apPilgrimStartYSlider = ConfigSlider
            { _csCurrent = 0
            , _csMin = 0
            , _csMax = 32
            , _csChangeRate = 1
            , _csCaption = "Pilgrim start position Y"
            }
        , _apColorConfig = def
        }

instance FromJSON Parameters where
    parseJSON = withObject "Parameters" $ \v -> Parameters
        <$> v .:? "grid_columns_slider" .!= _apGridColumnsSlider
        <*> v .:? "grid_rows_slider" .!= _apGridRowsSlider
        <*> v .:? "grid_animation_slider" .!= _apGridAnimationSlider
        <*> v .:? "grid_replay_slider" .!= _apGridReplaySlider
        <*> v .:? "link_to_node_slider" .!= _apLinkToNodeSlider
        <*> v .:? "node_to_width_slider" .!= _apNodeToWidthSlider
        <*> v .:? "pilgrim_start_x" .!= _apPilgrimStartXSlider
        <*> v .:? "pilgrim_start_y" .!= _apPilgrimStartYSlider
        <*> v .:? "color_config" .!= _apColorConfig where
            Parameters{..} = def

instance ToJSON Parameters where
    toJSON Parameters{..} = object
        [ "grid_columns_slider" .= _apGridColumnsSlider
        , "grid_rows_slider" .= _apGridRowsSlider
        , "grid_animation_slider" .= _apGridAnimationSlider
        , "grid_replay_slider" .= _apGridReplaySlider
        , "link_to_node_slider" .= _apLinkToNodeSlider
        , "node_to_width_slider" .= _apNodeToWidthSlider
        , "pilgrim_start_x" .= _apPilgrimStartXSlider
        , "pilgrim_start_y" .= _apPilgrimStartYSlider
        , "color_config" .= _apColorConfig
        ]

instance FromFile Parameters
instance ToFile Parameters

gameFromParameters :: Parameters -> Game
gameFromParameters Parameters{..} = game where
    game = makeGame_ (floor gridColumns) (floor gridRows) $ def
        { _position = (max 0 $ floor x, max 0 $ floor y)
        }
    gridColumns = _csCurrent _apGridColumnsSlider
    gridRows = _csCurrent _apGridRowsSlider
    x = min (gridColumns-1) $ _csCurrent _apPilgrimStartXSlider
    y = min (gridRows-1) $ _csCurrent _apPilgrimStartYSlider

parametersFromGame :: Game -> Parameters -> Parameters
parametersFromGame Game{..} p = p
    & gridColumnsSlider . currentValue .~ cols'
    & gridRowsSlider . currentValue .~ rows' where
        (cols, rows) = getBounds _grid
        cols' = fromIntegral $ cols+1
        rows' = fromIntegral $ rows+1
