{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Parameters.ConfigSlider
    ( ConfigSlider(..)
    , csCurrent
    , csMin
    , csMax
    , csCaption
    ) where

import Control.Lens
import Data.Aeson
import Data.Text (Text)

data ConfigSlider = ConfigSlider
    { _csCurrent :: Double
    , _csMin :: Double
    , _csMax :: Double
    , _csCaption :: Text
    } deriving (Eq, Show)

instance FromJSON ConfigSlider where
    parseJSON = withObject "ConfigSlider" $ \v -> ConfigSlider
        <$> v .: "default"
        <*> v .: "min"
        <*> v .: "max"
        <*> v .: "caption"

makeLenses 'ConfigSlider