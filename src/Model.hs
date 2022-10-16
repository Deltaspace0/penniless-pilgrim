{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Model
    ( module Event
    , module Game
    , module Parameters
    , AppModel(..)
    , initialGame
    , currentGame
    , showConfig
    , parameters
    , initModel
    , handleEvent
    ) where

import Control.Lens
import Data.Default
import Monomer

import Event
import Game
import Parameters

data AppModel = AppModel
    { _appInitialGame :: Game
    , _appCurrentGame :: Game
    , _appShowConfig  :: Bool
    , _appParameters  :: AppParameters
    } deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

makeLensesWith abbreviatedFields 'AppModel

initModel :: Game -> AppModel
initModel game = AppModel
    { _appInitialGame = game
    , _appCurrentGame = game
    , _appShowConfig  = False
    , _appParameters  = def
    }

movePilgrimHandle :: Direction -> EventHandle
movePilgrimHandle d model = [Model $ model & updateGame] where
    updateGame = currentGame %~ movePilgrim d

resetPilgrimHandle :: EventHandle
resetPilgrimHandle model = [Model $ model & updateGame] where
    updateGame = currentGame .~ model ^. initialGame

toggleConfigHandle :: EventHandle
toggleConfigHandle model = [Model $ model & showConfig %~ not]

resizeGridHandle :: EventHandle
resizeGridHandle model = [Model $ model & updateGame] where
    updateGame = currentGame .~ makeGame (floor gc) (floor gr)
    gc = model ^. parameters . gridColumnsSlider . csCurrent
    gr = model ^. parameters . gridRowsSlider . csCurrent

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent wenv node model evt = case evt of
    AppInit       -> [SetFocusOnKey "mainGrid"]
    MovePilgrim d -> movePilgrimHandle d model
    ResetPilgrim  -> resetPilgrimHandle model
    ToggleConfig  -> toggleConfigHandle model
    ResizeGrid    -> resizeGridHandle model