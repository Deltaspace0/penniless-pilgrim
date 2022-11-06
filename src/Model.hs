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
    , saveConfigCaption
    , initModel
    , handleEvent
    , gameFromParameters
    ) where

import Control.Lens
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
    | AppToggleConfig
    | AppSaveConfig
    | AppSetSaveConfigCaption Text
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
    , _appSaveConfigCaption :: Text
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
    return $ AppModel
        { _appConfigPath = path
        , _appInitialGame = game
        , _appCurrentGame = game
        , _appShowConfig = False
        , _appParameters = parameters'
        , _appNextTax = Nothing
        , _appInitialSaveConfigCaption = saveConfigCaption'
        , _appSaveConfigCaption = saveConfigCaption'
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

toggleConfigHandle :: EventHandle
toggleConfigHandle model = [Model $ model & showConfig %~ not]

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

setSaveConfigCaptionHandle :: Text -> EventHandle
setSaveConfigCaptionHandle text model = [Model model'] where
    model' = model & saveConfigCaption .~ text

resizeGridHandle :: EventHandle
resizeGridHandle model = [Model model'] where
    model' = model & currentGame .~ game & initialGame .~ game
    game = gameFromParameters $ model ^. parameters

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent wenv node model evt = case evt of
    AppInit -> [SetFocusOnKey "mainGrid"]
    AppResetGame -> resetPilgrimHandle model
    AppToggleConfig -> toggleConfigHandle model
    AppSaveConfig -> saveConfigHandle model
    AppSetSaveConfigCaption t -> setSaveConfigCaptionHandle t model
    AppResizeGrid -> resizeGridHandle model

gameFromParameters :: AppParameters -> Game
gameFromParameters parameters' = game where
    game = makeGame (floor gridColumns) (floor gridRows)
    gridColumns = parameters' ^. gridColumnsSlider . csCurrent
    gridRows = parameters' ^. gridRowsSlider . csCurrent