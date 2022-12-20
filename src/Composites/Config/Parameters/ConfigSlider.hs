{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Composites.Config.Parameters.ConfigSlider
    ( ConfigSlider(..)
    , csCurrent
    , csMin
    , csMax
    , csChangeRate
    , csCaption
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

makeLenses 'ConfigSlider

instance FromJSON ConfigSlider where
    parseJSON = withObject "ConfigSlider" $ \v -> ConfigSlider
        <$> v .: "default"
        <*> v .: "min"
        <*> v .: "max"
        <*> v .: "change_rate"
        <*> v .: "caption"

instance ToJSON ConfigSlider where
    toJSON configSlider = object
        [ "default" .= (configSlider ^. csCurrent)
        , "min" .= (configSlider ^. csMin)
        , "max" .= (configSlider ^. csMax)
        , "change_rate" .= (configSlider ^. csChangeRate)
        , "caption" .= (configSlider ^. csCaption)
        ]