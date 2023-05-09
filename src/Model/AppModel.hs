{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.Game
    , AppModel(..)
    , activeMenu
    , parameters
    , initGame
    , gameSaves
    , gameSavesPath
    , parametersPath
    , nextTax
    , initModel
    , gameSavesCaptionMethod
    ) where

import Control.Lens
import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)
import Monomer.SaveManager
import TextShow
import qualified Data.Text as T

import Model.AppMenu
import Model.File
import Model.Game
import Model.Parameters

data AppModel = AppModel
    { _appActiveMenu :: Maybe AppMenu
    , _appParameters :: Parameters
    , _appInitGame :: Game
    , _appGameSaves :: SaveManagerModel Game
    , _appGameSavesPath :: Maybe String
    , _appParametersPath :: Maybe String
    , _appNextTax :: Maybe Double
    } deriving Eq

makeLensesWith abbreviatedFields 'AppModel

initModel :: Maybe String -> Maybe String -> IO AppModel
initModel configPath gamesPath = do
    parameters' <- fromMaybeFile configPath
    savedGames <- fromMaybeFile gamesPath
    let game = gameFromParameters parameters'
        gameSaves' = initSaveManagerModel game
    return $ AppModel
        { _appActiveMenu = Nothing
        , _appParameters = parameters'
        , _appInitGame = game
        , _appGameSaves = gameSaves' & savedData .~ savedGames
        , _appGameSavesPath = gamesPath
        , _appParametersPath = configPath
        , _appNextTax = Nothing
        }

gameSavesCaptionMethod :: Game -> ZonedTime -> Text
gameSavesCaptionMethod Game{..} time = caption where
    caption = cols' <> "x" <> rows'
        <> "\t\tTax: " <> (showt _tax)
        <> "\t\tX: " <> (showt $ fst _position)
        <> "\tY: " <> (showt $ snd _position)
        <> "\t\t" <> (T.pack $ show time)
    Pilgrim{..} = _pilgrim
    (cols', rows') = (showt $ cols+1, showt $ rows+1)
    (cols, rows) = getBounds _grid
