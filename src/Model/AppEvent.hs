{-# LANGUAGE OverloadedStrings #-}

module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Exception
import Control.Lens
import Data.Aeson.Encode.Pretty
import Data.Foldable
import Data.Maybe
import Monomer
import Monomer.SaveManager
import qualified Data.ByteString.Lazy.UTF8 as BLU

import Composites
import Model.AppModel
import Model.Game

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
        & gameSaves . initData .~ game
        & gameSaves . currentData .~ game'
    game = gameFromConfig $ model ^. configModel
    game' = fromMaybe game $ applyPath directions game
    directions = _path $ _pilgrim $ model ^. gameSaves . currentData

setGameHandle :: Game -> EventHandle
setGameHandle game model = [Model model'] where
    model' = model & updateGame & updateConfig
    updateGame = gameSaves . currentData .~ game
    updateConfig = configModel %~ configFromGame game

saveGamesToFileHandle :: Saves Game -> EventHandle
saveGamesToFileHandle games model = [Producer handler] where
    handler _ = if null path
        then return ()
        else tryOperation >> return ()
    tryOperation = try operation :: IO (Either SomeException ())
    operation = writeFile (fromJust path) contents
    path = model ^. gameSavesPath
    contents = BLU.toString $ encodePretty $ toList games