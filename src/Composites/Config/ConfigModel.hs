{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Composites.Config.ConfigModel
    ( module Composites.Config.Parameters
    , ConfigModel(..)
    , filePath
    , initialSaveCaption
    , initialLoadCaption
    , saveCaption
    , loadCaption
    , parameters
    , configFromGame
    , gameFromConfig
    , initConfigModel
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)

import Composites.Config.Parameters
import Model.Game

data ConfigModel = ConfigModel
    { _cfgFilePath :: Maybe String
    , _cfgInitialSaveCaption :: Text
    , _cfgInitialLoadCaption :: Text
    , _cfgSaveCaption :: Text
    , _cfgLoadCaption :: Text
    , _cfgParameters :: Parameters
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'ConfigModel

configFromGame :: Game -> ConfigModel -> ConfigModel
configFromGame game model = model
    & parameters . gridColumnsSlider . csCurrent .~ cols'
    & parameters . gridRowsSlider . csCurrent .~ rows' where
        (cols, rows) = _gridBounds $ _grid game
        cols' = fromIntegral $ cols+1
        rows' = fromIntegral $ rows+1

gameFromConfig :: ConfigModel -> Game
gameFromConfig model = gameFromParameters $ model ^. parameters

initConfigModel :: Maybe String -> IO ConfigModel
initConfigModel path = do
    let f = fmap snd . parametersFromFile
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
