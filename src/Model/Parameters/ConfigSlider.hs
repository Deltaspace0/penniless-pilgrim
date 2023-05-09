{-# LANGUAGE RecordWildCards #-}

module Model.Parameters.ConfigSlider
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
    toJSON ConfigSlider{..} = object
        [ "default" .= _csCurrent
        , "min" .= _csMin
        , "max" .= _csMax
        , "change_rate" .= _csChangeRate
        , "caption" .= _csCaption
        ]

currentValue :: Lens' ConfigSlider Double
currentValue = lens getter setter where
    getter = _csCurrent
    setter configSlider newValue = configSlider
        { _csCurrent = newValue
        }
