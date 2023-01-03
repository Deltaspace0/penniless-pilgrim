{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Composites.Config.Model
    ( module Composites.Config.Parameters
    , ConfigEvent(..)
    , ConfigModel(..)
    , filePath
    , initialSaveCaption
    , initialLoadCaption
    , saveCaption
    , loadCaption
    , parameters
    , onGridDimensionsChange
    , configFromGame
    , gameFromConfig
    , initConfigModel
    , handleConfigEvent
    ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Monomer

import Composites.Config.Parameters
import Model.Game

data ConfigEvent
    = ConfigSave
    | ConfigLoad
    | ConfigSetSaveCaption Text
    | ConfigSetLoadCaption Text
    | ConfigSetParameters Parameters
    | ConfigReportGameChange
    deriving (Eq, Show)

data ConfigModel ep = ConfigModel
    { _cfgFilePath :: Maybe String
    , _cfgInitialSaveCaption :: Text
    , _cfgInitialLoadCaption :: Text
    , _cfgSaveCaption :: Text
    , _cfgLoadCaption :: Text
    , _cfgParameters :: Parameters
    , _cfgOnGridDimensionsChange :: ep
    } deriving (Eq, Show)

type EventHandle sp ep = ConfigModel ep ->
    [EventResponse (ConfigModel ep) ConfigEvent sp ep]

makeLensesWith abbreviatedFields 'ConfigModel

configFromGame :: Game -> ConfigModel ep -> ConfigModel ep
configFromGame game model = model
    & parameters . gridColumnsSlider . csCurrent .~ cols'
    & parameters . gridRowsSlider . csCurrent .~ rows' where
        (cols, rows) = getBounds $ _grid game
        cols' = fromIntegral $ cols+1
        rows' = fromIntegral $ rows+1

gameFromConfig :: ConfigModel ep -> Game
gameFromConfig model = gameFromParameters $ model ^. parameters

initConfigModel :: ep -> Maybe String -> IO (ConfigModel ep)
initConfigModel onGridDimensionsChange path = do
    parameters' <- if null path
        then return def
        else snd <$> parametersFromFile (fromJust path)
    let saveCaption' = "Save config to file"
        loadCaption' = "Load config from file"
    return $ ConfigModel
        { _cfgFilePath = path
        , _cfgInitialSaveCaption = saveCaption'
        , _cfgInitialLoadCaption = loadCaption'
        , _cfgSaveCaption = saveCaption'
        , _cfgLoadCaption = loadCaption'
        , _cfgParameters = parameters'
        , _cfgOnGridDimensionsChange = onGridDimensionsChange
        }

handleConfigEvent :: EventHandler (ConfigModel ep) ConfigEvent sp ep
handleConfigEvent _ _ model event = case event of
    ConfigSave -> saveHandle model
    ConfigLoad -> loadHandle model
    ConfigSetSaveCaption text -> setSaveCaptionHandle text model
    ConfigSetLoadCaption text -> setLoadCaptionHandle text model
    ConfigSetParameters p -> setParametersHandle p model
    ConfigReportGameChange -> reportGameChangeHandle model

saveHandle :: EventHandle sp ep
saveHandle model = [Task taskHandler] where
    taskHandler = do
        let path = model ^. filePath
            parameters' = model ^. parameters
        success <- if null path
            then return False
            else parametersToFile parameters' $ fromJust path
        return $ ConfigSetSaveCaption $ if success
            then "Successfully saved config to file"
            else "Couldn't save config to file"

loadHandle :: EventHandle sp ep
loadHandle model = [Producer producerHandler] where
    producerHandler raiseEvent = do
        let path = model ^. filePath
        (success, parameters') <- if null path
            then return (False, def)
            else parametersFromFile $ fromJust path
        let model' = model & parameters .~ parameters'
            caption = if success
                then "Successfully loaded config from file"
                else "Couldn't load config from file"
        when success $ do
            raiseEvent $ ConfigSetParameters parameters'
            raiseEvent ConfigReportGameChange
        raiseEvent $ ConfigSetLoadCaption caption

setSaveCaptionHandle :: Text -> EventHandle sp ep
setSaveCaptionHandle text model = [Model model'] where
    model' = model & saveCaption .~ text

setLoadCaptionHandle :: Text -> EventHandle sp ep
setLoadCaptionHandle text model = [Model model'] where
    model' = model & loadCaption .~ text

setParametersHandle :: Parameters -> EventHandle sp ep
setParametersHandle parameters' model = [Model model'] where
    model' = model & parameters .~ parameters'

reportGameChangeHandle :: EventHandle sp ep
reportGameChangeHandle model = [Report event] where
    event = model ^. onGridDimensionsChange