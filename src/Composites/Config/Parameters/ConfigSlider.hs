{-# LANGUAGE OverloadedStrings #-}

module Composites.Config.Parameters.ConfigSlider
    ( ConfigSlider(..)
    , currentValue
    ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Text (Text)

data ConfigSlider = ConfigSlider
    { _csCurrent :: Double
    , _csMin :: Double
    , _csMax :: Double
    , _csChangeRate :: Double
    , _csCaption :: Text
    } deriving (Eq, Show)

instance FromJSON ConfigSlider where
    parseJSON = withObject "ConfigSlider" $ \v -> ConfigSlider
        <$> v .: "default"
        <*> v .: "min"
        <*> v .: "max"
        <*> v .: "change_rate"
        <*> v .: "caption"

instance ToJSON ConfigSlider where
    toJSON configSlider = object
        [ "default" .= (_csCurrent configSlider)
        , "min" .= (_csMin configSlider)
        , "max" .= (_csMax configSlider)
        , "change_rate" .= (_csChangeRate configSlider)
        , "caption" .= (_csCaption configSlider)
        ]

currentValue :: Lens' ConfigSlider Double
currentValue = lens getter setter where
    getter = _csCurrent
    setter configSlider newValue = configSlider
        { _csCurrent = newValue
        }
