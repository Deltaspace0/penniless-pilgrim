{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model
    ( module Model.Event
    , module Model.Game
    , module Model.Parameters
    , AppModel(..)
    , initialGame
    , currentGame
    , showConfig
    , parameters
    , nextTax
    , initModel
    , initModel_
    , handleEvent
    , gameFromParameters
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Monomer

import Model.Event
import Model.Game
import Model.Grid
import Model.Parameters

data AppModel = AppModel
    { _appInitialGame :: Game
    , _appCurrentGame :: Game
    , _appShowConfig :: Bool
    , _appParameters :: AppParameters
    , _appNextTax :: Maybe Double
    } deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

makeLensesWith abbreviatedFields 'AppModel

initModel :: AppModel
initModel = initModel_ def

initModel_ :: AppParameters -> AppModel
initModel_ p = AppModel
    { _appInitialGame = game
    , _appCurrentGame = game
    , _appShowConfig = False
    , _appParameters = p
    , _appNextTax = Nothing
    } where
        game = gameFromParameters p

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

resizeGridHandle :: EventHandle
resizeGridHandle model = [Model $ model & currentGame .~ game] where
    game = gameFromParameters $ model ^. parameters

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent wenv node model evt = case evt of
    AppInit -> [SetFocusOnKey "mainGrid"]
    ResetPilgrim -> resetPilgrimHandle model
    ToggleConfig -> toggleConfigHandle model
    ResizeGrid -> resizeGridHandle model

gameFromParameters :: AppParameters -> Game
gameFromParameters p = makeGame (floor gc) (floor gr) where
    gc = p ^. gridColumnsSlider . csCurrent
    gr = p ^. gridRowsSlider . csCurrent