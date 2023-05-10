{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Composites.Config.ConfigModel
    ( ConfigModel(..)
    , alertMessage
    , parameters
    , currentMenu
    , initConfigModel
    ) where

import Control.Lens
import Data.Text (Text)
import Data.Default

import Composites.Config.ConfigMenu
import Model.Parameters

data ConfigModel = ConfigModel
    { _cfgAlertMessage :: Maybe Text
    , _cfgParameters :: Parameters
    , _cfgCurrentMenu :: ConfigMenu
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'ConfigModel

initConfigModel :: ConfigModel
initConfigModel = ConfigModel
    { _cfgAlertMessage = Nothing
    , _cfgParameters = def
    , _cfgCurrentMenu = MainMenu
    }
