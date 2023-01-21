{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.AppModel
    ( module Model.Game
    , AppModel(..)
    , activeMenu
    , configModel
    , initGame
    , gameSaves
    , gameSavesPath
    , nextTax
    , initModel
    , gameSavesCaptionMethod
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Data.Time.LocalTime (ZonedTime)
import Monomer.SaveManager
import TextShow
import qualified Data.Text as T

import Composites
import Model.AppMenu
import Model.File
import Model.Game
import Model.Parameters

data AppModel = AppModel
    { _appActiveMenu :: Maybe AppMenu
    , _appConfigModel :: ConfigModel
    , _appInitGame :: Game
    , _appGameSaves :: SaveManagerModel Game
    , _appGameSavesPath :: Maybe String
    , _appNextTax :: Maybe Double
    } deriving Eq

makeLensesWith abbreviatedFields 'AppModel

initModel :: Maybe String -> Maybe String -> IO AppModel
initModel configPath gamesPath = do
    configModel' <- initConfigModel configPath
    let game = gameFromParameters $ configModel' ^. parameters
        gameSaves' = initSaveManagerModel game
        f = fmap snd . fromFile
    savedGames <- fromMaybe (pure def) $ f <$> gamesPath
    return $ AppModel
        { _appActiveMenu = Nothing
        , _appConfigModel = configModel'
        , _appInitGame = game
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
