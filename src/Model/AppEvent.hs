{-# LANGUAGE OverloadedStrings #-}

module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.Maybe
import Monomer
import Monomer.SaveManager

import Composites
import Model.AppModel
import Model.Game
import Model.File
import Model.Parameters

data AppEvent
    = AppInit
    | AppUpdateGameWithConfig
    | AppSetGame Game
    | AppSaveGamesToFile (Saves Game)
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> [SetFocusOnKey "mainGrid"]
    AppUpdateGameWithConfig -> updateGameWithConfigHandle model
    AppSetGame game -> setGameHandle game model
    AppSaveGamesToFile games -> saveGamesToFileHandle games model

updateGameWithConfigHandle :: EventHandle
updateGameWithConfigHandle model = [Model model'] where
    model' = model
        & initGame .~ game
        & gameSaves . currentData .~ game'
    game = gameFromParameters $ model ^. configModel . parameters
    game' = fromMaybe game $ transferPath oldGame game
    oldGame = model ^. gameSaves . currentData

setGameHandle :: Game -> EventHandle
setGameHandle game model = [Model model'] where
    model' = model & updateGame & updateConfig
    updateGame = gameSaves . currentData .~ game
    updateConfig = configModel . parameters %~ f
    f = parametersFromGame game

saveGamesToFileHandle :: Saves Game -> EventHandle
saveGamesToFileHandle games model = [Producer handler] where
    handler _ = fromMaybe (pure ()) $ (>> pure ()) <$> operation
    operation = toFile games <$> model ^. gameSavesPath
