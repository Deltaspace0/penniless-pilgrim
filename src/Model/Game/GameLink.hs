{-# LANGUAGE OverloadedStrings #-}

module Model.Game.GameLink
    ( GameLink(..)
    ) where

import Data.Aeson
import Data.Text

data GameLink
    = LinkBack
    | LinkForward
    deriving (Eq, Show)

instance FromJSON GameLink where
    parseJSON = withText "GameLink" $ \v -> return $ case v of
        "LinkBack" -> LinkBack
        "LinkForward" -> LinkForward

instance ToJSON GameLink where
    toJSON = String . pack . show
