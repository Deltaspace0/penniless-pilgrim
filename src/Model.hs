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
    | AppResetPilgrim
    | AppToggleConfig
    | AppSaveConfig
    | AppSaveConfigResult Bool
    | AppResizeGrid
    deriving (Eq, Show)

data AppModel = AppModel
    { _appConfigPath :: Maybe String
    , _appInitialGame :: Game
    , _appCurrentGame :: Game
    , _appShowConfig :: Bool
    , _appParameters :: AppParameters
    , _appNextTax :: Maybe Double
    } deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

makeLensesWith abbreviatedFields 'AppModel

initModel :: Maybe String -> IO AppModel
initModel path = do
    p <- if null path
        then return def
        else fromFile $ fromJust path
    let game = gameFromParameters p
    return $ AppModel
        { _appConfigPath = path
        , _appInitialGame = game
        , _appCurrentGame = game
        , _appShowConfig = False
        , _appParameters = p
        , _appNextTax = Nothing
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
        return $ AppSaveConfigResult success

saveConfigResultHandle :: Bool -> EventHandle
saveConfigResultHandle success model = if success
    then []
    else []

resizeGridHandle :: EventHandle
resizeGridHandle model = [Model $ model & currentGame .~ game] where
    game = gameFromParameters $ model ^. parameters

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent wenv node model evt = case evt of
    AppInit -> [SetFocusOnKey "mainGrid"]
    AppResetPilgrim -> resetPilgrimHandle model
    AppToggleConfig -> toggleConfigHandle model
    AppSaveConfig -> saveConfigHandle model
    AppSaveConfigResult s -> saveConfigResultHandle s model
    AppResizeGrid -> resizeGridHandle model

gameFromParameters :: AppParameters -> Game
gameFromParameters p = makeGame (floor gc) (floor gr) where
    gc = p ^. gridColumnsSlider . csCurrent
    gr = p ^. gridRowsSlider . csCurrent