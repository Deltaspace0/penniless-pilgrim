{-# LANGUAGE OverloadedStrings #-}

module Model.Game.Link
    ( Link(..)
    ) where

import Data.Aeson
import Data.Text

data Link
    = LinkBack
    | LinkForward
    deriving (Eq, Show)

instance FromJSON Link where
    parseJSON = withText "Link" $ \v -> return $ case v of
        "LinkBack" -> LinkBack
        "LinkForward" -> LinkForward

instance ToJSON Link where
    toJSON = String . pack . show