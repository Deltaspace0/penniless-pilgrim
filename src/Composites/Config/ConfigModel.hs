{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Composites.Config.ConfigModel
    ( ConfigModel(..)
    , filePath
    , alertMessage
    , parameters
    , initConfigModel
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Model.File
import Model.Parameters

data ConfigModel = ConfigModel
    { _cfgFilePath :: Maybe String
    , _cfgAlertMessage :: Maybe Text
    , _cfgParameters :: Parameters
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'ConfigModel

initConfigModel :: Maybe String -> IO ConfigModel
initConfigModel path = do
    let f = fmap snd . fromFile
    parameters' <- fromMaybe (pure def) $ f <$> path
    return $ ConfigModel
        { _cfgFilePath = path
        , _cfgAlertMessage = Nothing
        , _cfgParameters = parameters'
        }
