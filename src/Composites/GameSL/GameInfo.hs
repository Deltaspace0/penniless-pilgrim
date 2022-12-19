{-# LANGUAGE OverloadedStrings #-}

module Composites.GameSL.GameInfo
    ( GameInfo(..)
    , fromGame
    , fromFile
    , toFile
    ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Foldable
import Data.Maybe
import Data.Sequence (Seq(..))
import Data.Text hiding (null)
import Data.Time.LocalTime
import Model.Game
import TextShow
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.Sequence as Seq

data GameInfo = GameInfo Game Text deriving (Eq, Show)

instance FromJSON GameInfo where
    parseJSON = withObject "GameInfo" $ \v -> GameInfo
        <$> v .: "game"
        <*> v .: "caption"

instance ToJSON GameInfo where
    toJSON (GameInfo game caption) = object
        [ "game" .= game
        , "caption" .= caption
        ]

fromGame :: Game -> IO GameInfo
fromGame game = do
    utcTime <- getZonedTime
    let (cols, rows) = getBounds $ _grid game
        cols' = showt $ cols+1
        rows' = showt $ rows+1
        pilgrim = _pilgrim game
        caption = cols' <> "x" <> rows'
            <> "\t\tTax: " <> (showt $ _tax pilgrim)
            <> "\t\tX: " <> (showt $ fst $ _position pilgrim)
            <> "\tY: " <> (showt $ snd $ _position pilgrim)
            <> "\t\t" <> (pack $ show utcTime)
    return $ GameInfo game caption

fromFile :: String -> IO (Bool, Seq GameInfo)
fromFile path = do
    let handler = const $ return "" :: SomeException -> IO String
    file <- catch (readFile path) handler
    let contents = BLU.fromString file
        savedGames = decode contents :: Maybe [GameInfo]
    return $ if null savedGames
        then (False, Seq.empty)
        else (True, Seq.fromList $ fromJust savedGames)

toFile :: Seq GameInfo -> String -> IO Bool
toFile savedGames path = do
    let converted = encodePretty $ toList savedGames
        contents = BLU.toString converted
        operation = writeFile path contents
    result <- try operation :: IO (Either SomeException ())
    case result of
        Left _ -> return False
        Right _ -> return True