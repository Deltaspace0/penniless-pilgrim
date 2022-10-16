{-# LANGUAGE TemplateHaskell #-}

module ConfigSlider
    ( ConfigSlider(..)
    , csCurrent
    , csMin
    , csMax
    , csCaption
    ) where

import Control.Lens
import Data.Text (Text)

data ConfigSlider = ConfigSlider
    { _csCurrent :: Double
    , _csMin     :: Double
    , _csMax     :: Double
    , _csCaption   :: Text
    } deriving (Eq, Show)

makeLenses 'ConfigSlider