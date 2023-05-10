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

import Composites.Config.ConfigCfg
import Composites.Config.ConfigModel
import Model.Parameters
import Util

data ConfigEvent
    = ConfigSave
    | ConfigLoad
    | ConfigSetMessage (Maybe Text)
    | ConfigSetParameters Parameters
    | ConfigReportGameChange
    deriving (Eq, Show)

type EventHandle sp ep = (ConfigCfg ep) -> ConfigModel ->
    [EventResponse ConfigModel ConfigEvent sp ep]

handleEvent
    :: (WidgetData sp Parameters)
    -> (ConfigCfg ep)
    -> EventHandler ConfigModel ConfigEvent sp ep
handleEvent wd config _ _ model event = case event of
    ConfigSave -> saveHandle config model
    ConfigLoad -> loadHandle config model
    ConfigSetMessage m -> setMessageHandle m config model
    ConfigSetParameters p -> setParametersHandle p wd config model
    ConfigReportGameChange -> reportGameChangeHandle config model

saveHandle :: EventHandle sp ep
saveHandle config model = [Task taskHandler] where
    taskHandler = do
        let f = toFile $ model ^. parameters
            filePath = _ccFilePath config
        success <- fromMaybe (pure False) $ f <$> filePath
        return $ ConfigSetMessage $ Just $ if success
            then "Successfully saved config to file"
            else "Couldn't save config to file"

loadHandle :: EventHandle sp ep
loadHandle config _ = [Producer producerHandler] where
    producerHandler raiseEvent = do
        let x = fromFile <$> _ccFilePath config
        (success, parameters') <- fromMaybe (pure (False, def)) x
        let caption = if success
                then "Successfully loaded config from file"
                else "Couldn't load config from file"
        when success $ do
            raiseEvent $ ConfigSetParameters parameters'
            raiseEvent ConfigReportGameChange
        raiseEvent $ ConfigSetMessage $ Just caption

setMessageHandle :: Maybe Text -> EventHandle sp ep
setMessageHandle alertMessage' _ model = [Model model'] where
    model' = model & alertMessage .~ alertMessage'

setParametersHandle
    :: Parameters
    -> (WidgetData sp Parameters)
    -> EventHandle sp ep
setParametersHandle p wdata _ _ = response where
    response = RequestParent <$> (widgetDataSet wdata p)

reportGameChangeHandle :: EventHandle sp ep
reportGameChangeHandle config _ = fromMaybe [] response where
    response = pure . Report <$> _ccGameChange config
