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
    , gameSaves
    , gameSavesPath
    , nextTax
    , initModel
    , handleEvent
    , gameSavesCaptionMethod
    ) where

import Control.Exception
import Control.Lens
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Foldable
import Data.Maybe
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)
import Monomer
import Monomer.SaveManager
import TextShow
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import Composites
import Model.Game
import Model.Grid

data AppMenu
    = ConfigMenu
    | GameSavesMenu
    deriving (Eq, Show)

data AppEvent
    = AppInit
    | AppResizeGrid
    | AppSetGame Game
    | AppSaveGamesToFile (Saves Game)
    deriving (Eq, Show)

data AppModel = AppModel
    { _appActiveMenu :: Maybe AppMenu
    , _appConfigModel :: ConfigModel AppEvent
    , _appGameSaves :: SaveManagerModel Game
    , _appGameSavesPath :: Maybe String
    , _appNextTax :: Maybe Double
    } deriving Eq

type EventHandle = AppModel -> [AppEventResponse AppModel AppEvent]

makeLensesWith abbreviatedFields 'AppModel

initModel :: Maybe String -> Maybe String -> IO AppModel
initModel configPath gamesPath = do
    configModel' <- initConfigModel AppResizeGrid configPath
    let game = gameFromConfig configModel'
        gameSaves' = initSaveManagerModel game
        handler = const $ return "" :: SomeException -> IO String
    file <- catch (readFile $ fromJust gamesPath) handler
    let contents = BLU.fromString file
        decoded = decode contents :: Maybe (Saves Game)
        savedGames = if null gamesPath || null decoded
            then Seq.empty
            else fromJust decoded
    return $ AppModel
        { _appActiveMenu = Nothing
        , _appConfigModel = configModel'
        , _appGameSaves = gameSaves' & savedData .~ savedGames
        , _appGameSavesPath = gamesPath
        , _appNextTax = Nothing
        }

gameSavesCaptionMethod :: Game -> ZonedTime -> Text
gameSavesCaptionMethod game time = caption where
    caption = cols' <> "x" <> rows'
        <> "\t\tTax: " <> (showt $ _tax pilgrim)
        <> "\t\tX: " <> (showt $ fst $ _position pilgrim)
        <> "\tY: " <> (showt $ snd $ _position pilgrim)
        <> "\t\t" <> (T.pack $ show time)
    pilgrim = _pilgrim game
    cols' = showt $ cols+1
    rows' = showt $ rows+1
    (cols, rows) = getBounds $ _grid game

resizeGridHandle :: EventHandle
resizeGridHandle model = [Model model'] where
    model' = model
        & gameSaves . initData .~ game
        & gameSaves . currentData .~ game'
    game = gameFromConfig $ model ^. configModel
    game' = if null gameWithAppliedPath
        then game
        else fromJust gameWithAppliedPath
    gameWithAppliedPath = applyPath directions game
    directions = _path $ _pilgrim currentGame
    currentGame = model ^. gameSaves . currentData

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

handleEvent :: AppEventHandler AppModel AppEvent
handleEvent _ _ model event = case event of
    AppInit -> [SetFocusOnKey "mainGrid"]
    AppResizeGrid -> resizeGridHandle model
    AppSetGame game -> setGameHandle game model
    AppSaveGamesToFile games -> saveGamesToFileHandle games model