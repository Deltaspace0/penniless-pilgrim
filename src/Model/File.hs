{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.File
    ( FromFile
    , ToFile
    , fromFile
    , fromMaybeFile
    , toFile
    ) where

import Control.Exception
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Default
import Data.Maybe
import Model.Game
import Monomer.SaveManager
import qualified Data.ByteString.Lazy.UTF8 as BLU

class (Default a, FromJSON a) => FromFile a where
    fromFile :: String -> IO (Bool, a)
    fromFile path = do
        let handler = const $ pure "" :: SomeException -> IO String
        file <- catch (readFile path) handler
        let contents = BLU.fromString file
            parameters = decode contents :: Maybe a
        return $ fromMaybe (False, def) $ (,) True <$> parameters
    fromMaybeFile :: Maybe String -> IO a
    fromMaybeFile path = if null path
        then return def
        else snd <$> (fromFile $ fromJust path)

class (ToJSON a) => ToFile a where
    toFile :: a -> String -> IO Bool
    toFile v path = do
        let config = defConfig {confCompare = flip compare}
            converted = encodePretty' config v
            contents = BLU.toString converted
            operation = writeFile path contents
        result <- try operation :: IO (Either SomeException ())
        case result of
            Left _ -> return False
            Right _ -> return True

instance FromFile (Saves Game)
instance ToFile (Saves Game)
