module Model.AppEvent
    ( AppEvent(..)
    , handleEvent
    ) where

import Control.Lens
import Data.Maybe
import Monomer
import Monomer.SaveManager

import Common.File
import Composites.GameControl
import Model.AppModel

data AppEvent
    = AppInit
    | AppUpdateGameWithConfig
    | AppSetGame Game
    | AppSaveGamesToFile (Saves Game)
    | AppHideMenu
    | AppSetReplay Bool
    deriving (Eq, Show)

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> [SetFocusOnKey "mainGrid"]
    AppUpdateGameWithConfig -> updateGameWithConfigHandle model
    AppSetGame game -> setGameHandle game model
    AppSaveGamesToFile games -> saveGamesToFileHandle games model
    AppHideMenu -> hideMenuHandle model
    AppSetReplay v -> setReplayHandle v model

updateGameWithConfigHandle :: EventHandle
updateGameWithConfigHandle model = [Model model'] where
    model' = model
        & initGame .~ game
        & gameSaves . currentData .~ game'
    game = gameFromParameters $ model ^. parameters
    game' = fromMaybe game $ transferPath oldGame game
    oldGame = model ^. gameSaves . currentData

setGameHandle :: Game -> EventHandle
setGameHandle game model = [Model model'] where
    model' = model
        & gameSaves . currentData .~ game
        & parameters %~ parametersFromGame game

saveGamesToFileHandle :: Saves Game -> EventHandle
saveGamesToFileHandle games model = [Producer handler] where
    handler _ = fromMaybe (pure ()) $ (>> pure ()) <$> operation
    operation = toFile games <$> model ^. gameSavesPath

hideMenuHandle :: EventHandle
hideMenuHandle model = [Model $ model & activeMenu .~ Nothing]

setReplayHandle :: Bool -> EventHandle
setReplayHandle v model =
    [ Model $ model & replaying .~ v
    , Message "mainGrid" $ EventSetReplay v
    ]
