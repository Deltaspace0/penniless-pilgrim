{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model
    ( module Model.Game
    , AppMenu(..)
    , AppEvent(..)
    , AppModel(..)
    , activeMenu
    , configModel
    , gameSLModel
    , nextTax
    , initModel
    , handleEvent
    ) where

import Control.Lens
import Data.Maybe
import Monomer

import Composites
import Model.Game
import Model.Grid

data AppMenu
    = ConfigMenu
    | GameSLMenu
    deriving (Eq, Show)

data AppEvent
    = AppInit
    | AppResizeGrid
    | AppSetGame Game
    deriving (Eq, Show)

data AppModel = AppModel
    { _appActiveMenu :: Maybe AppMenu
    , _appConfigModel :: ConfigModel AppEvent
    , _appGameSLModel :: GameSLModel
    , _appNextTax :: Maybe Double
    } deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

makeLensesWith abbreviatedFields 'AppModel

initModel :: Maybe String -> Maybe String -> IO AppModel
initModel configPath gamesPath = do
    configModel' <- initConfigModel AppResizeGrid configPath
    let game = gameFromConfig configModel'
    gameSLModel' <- initGameSLModel game gamesPath
    return $ AppModel
        { _appActiveMenu = Nothing
        , _appConfigModel = configModel'
        , _appGameSLModel = gameSLModel'
        , _appNextTax = Nothing
        }

resizeGridHandle :: EventHandle
resizeGridHandle model = [Model model'] where
    model' = model
        & gameSLModel . initialGame .~ game
        & gameSLModel . currentGame .~ game'
    game = gameFromConfig $ model ^. configModel
    game' = if null gameWithAppliedPath
        then game
        else fromJust gameWithAppliedPath
    gameWithAppliedPath = applyPath directions game
    directions = _path $ _pilgrim currentGame'
    currentGame' = model ^. gameSLModel . currentGame

setGameHandle :: Game -> EventHandle
setGameHandle game model = [Model model'] where
    model' = model & updateGame & updateConfig
    updateGame = gameSLModel . currentGame .~ game
    updateConfig = configModel %~ configFromGame game

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> [SetFocusOnKey "mainGrid"]
    AppResizeGrid -> resizeGridHandle model
    AppSetGame game -> setGameHandle game model