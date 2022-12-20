{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Composites.GameSL.Model
    ( module Composites.GameSL.GameInfo
    , GameSLEvent(..)
    , GameSLModel(..)
    , savedGames
    , initialGame
    , currentGame
    , selectedGame
    , initGameSLModel
    , handleGameSLEvent
    ) where

import Control.Lens
import Data.Maybe
import Data.Sequence (Seq(..))
import Monomer
import qualified Data.Sequence as Seq

import Composites.GameSL.GameInfo
import Model.Game

data GameSLEvent
    = GameSLNewSlot
    | GameSLSave
    | GameSLLoad
    | GameSLRemove
    | GameSLSetSavedGames (Seq GameInfo)
    | GameSLSetSelectedGame (Maybe GameInfo)
    | GameSLSyncToFile
    deriving (Eq, Show)

data GameSLModel = GameSLModel
    { _gslFilePath :: Maybe String
    , _gslSavedGames :: Seq GameInfo
    , _gslInitialGame :: Game
    , _gslCurrentGame :: Game
    , _gslSelectedGame :: Maybe GameInfo
    } deriving (Eq, Show)

type EventHandle sp ep = GameSLModel ->
    [EventResponse GameSLModel GameSLEvent sp ep]

makeLensesWith abbreviatedFields 'GameSLModel

initGameSLModel :: Game -> Maybe String -> IO GameSLModel
initGameSLModel game path = do
    savedGames' <- if null path
        then return Seq.empty
        else snd <$> gameInfoFromFile (fromJust path)
    return $ GameSLModel
        { _gslFilePath = path
        , _gslSavedGames = savedGames'
        , _gslInitialGame = game
        , _gslCurrentGame = game
        , _gslSelectedGame = Nothing
        }

handleGameSLEvent :: EventHandler GameSLModel GameSLEvent sp ep
handleGameSLEvent _ _ model event = case event of
    GameSLNewSlot -> newSlotHandle model
    GameSLSave -> saveHandle model
    GameSLLoad -> loadHandle model
    GameSLRemove -> removeHandle model
    GameSLSetSavedGames s -> setSavedGamesHandle s model
    GameSLSetSelectedGame s -> setSelectedGameHandle s model
    GameSLSyncToFile -> syncToFileHandle model

newSlotHandle :: EventHandle sp ep
newSlotHandle model = [Producer producerHandler] where
    producerHandler raiseEvent = do
        gameInfo <- fromGame $ model ^. currentGame
        let savedGames' = gameInfo <| (model ^. savedGames)
        raiseEvent $ GameSLSetSavedGames savedGames'
        raiseEvent $ GameSLSetSelectedGame $ Just gameInfo

saveHandle :: EventHandle sp ep
saveHandle model = [Producer producerHandler | selected] where
    producerHandler raiseEvent = do
        newGameInfo <- fromGame $ model ^. currentGame
        let savedGames' = model ^. savedGames
            i = Seq.elemIndexL (fromJust gameInfo) savedGames'
            ix = fromJust i
            savedGames'' = Seq.update ix newGameInfo savedGames'
        raiseEvent $ GameSLSetSavedGames savedGames''
        raiseEvent $ GameSLSetSelectedGame $ Just newGameInfo
    gameInfo = model ^. selectedGame
    selected = not $ null gameInfo

loadHandle :: EventHandle sp ep
loadHandle model = [Model model' | not $ null gameInfo] where
    model' = model & currentGame .~ game
    GameInfo game _ = fromJust gameInfo
    gameInfo = model ^. selectedGame

removeHandle :: EventHandle sp ep
removeHandle model = responses where
    responses = if null gameInfo
        then []
        else [Model model', Event GameSLSyncToFile]
    model' = model
        & selectedGame .~ next
        & savedGames .~ savedGames''
    savedGames'' = Seq.deleteAt (fromJust i) savedGames'
    savedGames' = model ^. savedGames
    i = Seq.elemIndexL (fromJust gameInfo) savedGames'
    next = if null savedGames''
        then Nothing
        else Just $ Seq.index savedGames'' 0
    gameInfo = model ^. selectedGame

setSavedGamesHandle :: Seq GameInfo -> EventHandle sp ep
setSavedGamesHandle savedGames' model = responses where
    responses = [Model model', Event GameSLSyncToFile]
    model' = model & savedGames .~ savedGames'

setSelectedGameHandle :: Maybe GameInfo -> EventHandle sp ep
setSelectedGameHandle gameInfo model = [Model model'] where
    model' = model & selectedGame .~ gameInfo

syncToFileHandle :: EventHandle sp ep
syncToFileHandle model = [Producer producerHandler] where
    producerHandler _ = do
        let path = model ^. filePath
            savedGames' = model ^. savedGames
        success <- if null path
            then return False
            else gameInfoToFile savedGames' $ fromJust path
        return ()