{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Composites.Config.ConfigModel
    ( ConfigModel(..)
    , alertMessage
    , parameters
    , initConfigModel
    ) where

import Control.Lens
import Data.Text (Text)
import Data.Default

import Model.Parameters

data ConfigModel = ConfigModel
    { _cfgAlertMessage :: Maybe Text
    , _cfgParameters :: Parameters
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'ConfigModel

initConfigModel :: ConfigModel
initConfigModel = ConfigModel
    { _cfgAlertMessage = Nothing
    , _cfgParameters = def
    }
