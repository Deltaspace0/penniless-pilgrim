{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model
    ( module Model.Game
    , module Model.Parameters
    , AppEvent(..)
    , AppModel(..)
    , initialGame
    , currentGame
    , showConfig
    , configModel
    , nextTax
    , initModel
    , handleEvent
    , gameFromParameters
    ) where

import Control.Lens
import Data.Maybe
import Monomer

import Composites
import Model.Game
import Model.Grid
import Model.Parameters

data AppEvent
    = AppInit
    | AppResizeGrid
    | AppSetGame Game
    deriving (Eq, Show)

data AppModel = AppModel
    { _appInitialGame :: Game
    , _appCurrentGame :: Game
    , _appShowConfig :: Bool
    , _appConfigModel :: ConfigModel AppEvent
    , _appNextTax :: Maybe Double
    } deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

makeLensesWith abbreviatedFields 'AppModel

initModel :: Maybe String -> IO AppModel
initModel path = do
    configModel' <- initConfigModel AppResizeGrid path
    let parameters' = configModel' ^. parameters
        game = gameFromParameters parameters'
    return $ AppModel
        { _appInitialGame = game
        , _appCurrentGame = game
        , _appShowConfig = False
        , _appConfigModel = configModel'
        , _appNextTax = Nothing
        }

resizeGridHandle :: EventHandle
resizeGridHandle model = [Model model'] where
    model' = model & initialGame .~ game & currentGame .~ game'
    game = gameFromParameters $ model ^. configModel . parameters
    game' = if null gameWithAppliedPath
        then game
        else fromJust gameWithAppliedPath
    gameWithAppliedPath = applyPath directions game
    directions = _path $ _pilgrim currentGame'
    currentGame' = model ^. currentGame

setGameHandle :: Game -> EventHandle
setGameHandle game model = [Model model'] where
    model' = model & updateGame & updateSliders
    updateSliders = updateColumns . updateRows
    updateColumns = currentValue gridColumnsSlider .~ cols'
    updateRows = currentValue gridRowsSlider .~ rows'
    updateGame = currentGame .~ game
    currentValue slider = parameters' . slider . csCurrent
    parameters' = configModel . parameters
    (cols, rows) = getBounds $ _grid game
    cols' = fromIntegral $ cols+1
    rows' = fromIntegral $ rows+1

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent wenv node model event = case event of
    AppInit -> [SetFocusOnKey "mainGrid"]
    AppResizeGrid -> resizeGridHandle model
    AppSetGame game -> setGameHandle game model

gameFromParameters :: AppParameters -> Game
gameFromParameters parameters' = game where
    game = makeGame (floor gridColumns) (floor gridRows)
    gridColumns = parameters' ^. gridColumnsSlider . csCurrent
    gridRows = parameters' ^. gridRowsSlider . csCurrent