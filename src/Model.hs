{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model
    ( module Model.Game
    , module Model.Parameters
    , AppEvent(..)
    , AppModel(..)
    , configPath
    , initialGame
    , currentGame
    , showConfig
    , parameters
    , nextTax
    , initialSaveConfigCaption
    , initialLoadConfigCaption
    , saveConfigCaption
    , loadConfigCaption
    , initModel
    , handleEvent
    , gameFromParameters
    ) where

import Control.Lens
import Control.Monad
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Monomer

import Model.Game
import Model.Grid
import Model.Parameters

data AppEvent
    = AppInit
    | AppResetGame
    | AppSaveConfig
    | AppLoadConfig
    | AppSetSaveConfigCaption Text
    | AppSetLoadConfigCaption Text
    | AppSetParameters AppParameters
    | AppResizeGrid
    deriving (Eq, Show)

data AppModel = AppModel
    { _appConfigPath :: Maybe String
    , _appInitialGame :: Game
    , _appCurrentGame :: Game
    , _appShowConfig :: Bool
    , _appParameters :: AppParameters
    , _appNextTax :: Maybe Double
    , _appInitialSaveConfigCaption :: Text
    , _appInitialLoadConfigCaption :: Text
    , _appSaveConfigCaption :: Text
    , _appLoadConfigCaption :: Text
    } deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

makeLensesWith abbreviatedFields 'AppModel

initModel :: Maybe String -> IO AppModel
initModel path = do
    parameters' <- if null path
        then return def
        else snd <$> fromFile (fromJust path)
    let game = gameFromParameters parameters'
        saveConfigCaption' = "Save config to file"
        loadConfigCaption' = "Load config from file"
    return $ AppModel
        { _appConfigPath = path
        , _appInitialGame = game
        , _appCurrentGame = game
        , _appShowConfig = False
        , _appParameters = parameters'
        , _appNextTax = Nothing
        , _appInitialSaveConfigCaption = saveConfigCaption'
        , _appInitialLoadConfigCaption = loadConfigCaption'
        , _appSaveConfigCaption = saveConfigCaption'
        , _appLoadConfigCaption = loadConfigCaption'
        }

resetPilgrimHandle :: EventHandle
resetPilgrimHandle model = [Model model'] where
    model' = model & updateGame & updateSliders
    updateSliders = updateColumns . updateRows
    updateColumns = currentValue gridColumnsSlider .~ cols'
    updateRows = currentValue gridRowsSlider .~ rows'
    updateGame = currentGame .~ model ^. initialGame
    currentValue slider = parameters . slider . csCurrent
    (cols, rows) = getBounds $ _grid $ model ^. initialGame
    cols' = fromIntegral $ cols+1
    rows' = fromIntegral $ rows+1

saveConfigHandle :: EventHandle
saveConfigHandle model = [Task taskHandler] where
    taskHandler = do
        let configPath' = model ^. configPath
            parameters' = model ^. parameters
        success <- if null configPath'
            then return False
            else toFile parameters' $ fromJust configPath'
        return $ AppSetSaveConfigCaption $ if success
            then "Successfully saved config to file"
            else "Couldn't save config to file"

loadConfigHandle :: EventHandle
loadConfigHandle model = [Producer producerHandler] where
    producerHandler raiseEvent = do
        let configPath' = model ^. configPath
        (success, parameters') <- if null configPath'
            then return (False, def)
            else fromFile $ fromJust configPath'
        let model' = model & parameters .~ parameters'
            caption = if success
                then "Successfully loaded config from file"
                else "Couldn't load config from file"
        when success $ do
            raiseEvent $ AppSetParameters parameters'
            raiseEvent AppResizeGrid
        raiseEvent $ AppSetLoadConfigCaption caption

setSaveConfigCaptionHandle :: Text -> EventHandle
setSaveConfigCaptionHandle text model = [Model model'] where
    model' = model & saveConfigCaption .~ text

setLoadConfigCaptionHandle :: Text -> EventHandle
setLoadConfigCaptionHandle text model = [Model model'] where
    model' = model & loadConfigCaption .~ text

setParametersHandle :: AppParameters -> EventHandle
setParametersHandle parameters' model = [Model model'] where
    model' = model & parameters .~ parameters'

resizeGridHandle :: EventHandle
resizeGridHandle model = [Model model'] where
    model' = model & initialGame .~ game & currentGame .~ game'
    game = gameFromParameters $ model ^. parameters
    game' = if currentGameBounds == bounds
        then currentGame'
        else game
    currentGameBounds = getBounds $ _grid currentGame'
    bounds = getBounds $ _grid game
    currentGame' = model ^. currentGame

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent wenv node model evt = case evt of
    AppInit -> [SetFocusOnKey "mainGrid"]
    AppResetGame -> resetPilgrimHandle model
    AppSaveConfig -> saveConfigHandle model
    AppLoadConfig -> loadConfigHandle model
    AppSetSaveConfigCaption t -> setSaveConfigCaptionHandle t model
    AppSetLoadConfigCaption t -> setLoadConfigCaptionHandle t model
    AppSetParameters p -> setParametersHandle p model
    AppResizeGrid -> resizeGridHandle model

gameFromParameters :: AppParameters -> Game
gameFromParameters parameters' = game where
    game = makeGame (floor gridColumns) (floor gridRows)
    gridColumns = parameters' ^. gridColumnsSlider . csCurrent
    gridRows = parameters' ^. gridRowsSlider . csCurrent