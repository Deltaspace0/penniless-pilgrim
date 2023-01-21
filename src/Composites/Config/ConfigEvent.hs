{-# LANGUAGE OverloadedStrings #-}

module Composites.Config.ConfigEvent
    ( ConfigEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Monomer

import Composites.Config.ConfigModel
import Model.File
import Model.Parameters

data ConfigEvent
    = ConfigSave
    | ConfigLoad
    | ConfigSetSaveCaption Text
    | ConfigSetLoadCaption Text
    | ConfigSetParameters Parameters
    | ConfigReportGameChange
    deriving (Eq, Show)

type EventHandle sp ep = ConfigModel ->
    [EventResponse ConfigModel ConfigEvent sp ep]

handleEvent
    :: Maybe ep
    -> EventHandler ConfigModel ConfigEvent sp ep
handleEvent onGameChange _ _ model event = case event of
    ConfigSave -> saveHandle model
    ConfigLoad -> loadHandle model
    ConfigSetSaveCaption text -> setSaveCaptionHandle text model
    ConfigSetLoadCaption text -> setLoadCaptionHandle text model
    ConfigSetParameters p -> setParametersHandle p model
    ConfigReportGameChange ->
        reportGameChangeHandle onGameChange model

saveHandle :: EventHandle sp ep
saveHandle model = [Task taskHandler] where
    taskHandler = do
        let f = toFile $ model ^. parameters
        success <- fromMaybe (pure False) $ f <$> model ^. filePath
        return $ ConfigSetSaveCaption $ if success
            then "Successfully saved config to file"
            else "Couldn't save config to file"

loadHandle :: EventHandle sp ep
loadHandle model = [Producer producerHandler] where
    producerHandler raiseEvent = do
        let x = fromFile <$> model ^. filePath
        (success, parameters') <- fromMaybe (pure (False, def)) x
        let caption = if success
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

reportGameChangeHandle :: Maybe ep -> EventHandle sp ep
reportGameChangeHandle onGameChange _ = fromMaybe [] response where
    response = pure . Report <$> onGameChange
