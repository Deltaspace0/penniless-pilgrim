{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Composites.Config.Model
    ( ConfigEvent(..)
    , ConfigModel(..)
    , filePath
    , initialSaveCaption
    , initialLoadCaption
    , saveCaption
    , loadCaption
    , parameters
    , onGridDimensionsChange
    , initConfigModel
    , handleConfigEvent
    ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Monomer

import Model.Parameters

data ConfigEvent
    = ConfigSave
    | ConfigLoad
    | ConfigSetSaveCaption Text
    | ConfigSetLoadCaption Text
    | ConfigSetParameters AppParameters
    | ConfigReportGDC
    deriving (Eq, Show)

data ConfigModel ep = ConfigModel
    { _cfgFilePath :: Maybe String
    , _cfgInitialSaveCaption :: Text
    , _cfgInitialLoadCaption :: Text
    , _cfgSaveCaption :: Text
    , _cfgLoadCaption :: Text
    , _cfgParameters :: AppParameters
    , _cfgOnGridDimensionsChange :: ep
    } deriving (Eq, Show)

type EventHandle sp ep = ConfigModel ep ->
    [EventResponse (ConfigModel ep) ConfigEvent sp ep]

makeLensesWith abbreviatedFields 'ConfigModel

initConfigModel :: ep -> Maybe String -> IO (ConfigModel ep)
initConfigModel onGridDimensionsChange path = do
    parameters' <- if null path
        then return def
        else snd <$> fromFile (fromJust path)
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
    ConfigReportGDC -> reportGDCHandle model

saveHandle :: EventHandle sp ep
saveHandle model = [Task taskHandler] where
    taskHandler = do
        let path = model ^. filePath
            parameters' = model ^. parameters
        success <- if null path
            then return False
            else toFile parameters' $ fromJust path
        return $ ConfigSetSaveCaption $ if success
            then "Successfully saved config to file"
            else "Couldn't save config to file"

loadHandle :: EventHandle sp ep
loadHandle model = [Producer producerHandler] where
    producerHandler raiseEvent = do
        let path = model ^. filePath
        (success, parameters') <- if null path
            then return (False, def)
            else fromFile $ fromJust path
        let model' = model & parameters .~ parameters'
            caption = if success
                then "Successfully loaded config from file"
                else "Couldn't load config from file"
        when success $ do
            raiseEvent $ ConfigSetParameters parameters'
            raiseEvent ConfigReportGDC
        raiseEvent $ ConfigSetLoadCaption caption

setSaveCaptionHandle :: Text -> EventHandle sp ep
setSaveCaptionHandle text model = [Model model'] where
    model' = model & saveCaption .~ text

setLoadCaptionHandle :: Text -> EventHandle sp ep
setLoadCaptionHandle text model = [Model model'] where
    model' = model & loadCaption .~ text

setParametersHandle :: AppParameters -> EventHandle sp ep
setParametersHandle parameters' model = [Model model'] where
    model' = model & parameters .~ parameters'

reportGDCHandle :: EventHandle sp ep
reportGDCHandle model = [Report onGridDimensionsChange'] where
    onGridDimensionsChange' = model ^. onGridDimensionsChange