{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Composites.Config.ConfigModel
    ( ConfigModel(..)
    , filePath
    , initialSaveCaption
    , initialLoadCaption
    , saveCaption
    , loadCaption
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
    , _cfgInitialSaveCaption :: Text
    , _cfgInitialLoadCaption :: Text
    , _cfgSaveCaption :: Text
    , _cfgLoadCaption :: Text
    , _cfgParameters :: Parameters
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'ConfigModel

initConfigModel :: Maybe String -> IO ConfigModel
initConfigModel path = do
    let f = fmap snd . fromFile
    parameters' <- fromMaybe (pure def) $ f <$> path
    let saveCaption' = "Save config to file"
        loadCaption' = "Load config from file"
    return $ ConfigModel
        { _cfgFilePath = path
        , _cfgInitialSaveCaption = saveCaption'
        , _cfgInitialLoadCaption = loadCaption'
        , _cfgSaveCaption = saveCaption'
        , _cfgLoadCaption = loadCaption'
        , _cfgParameters = parameters'
        }
