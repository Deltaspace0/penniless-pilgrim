{-# LANGUAGE OverloadedStrings #-}

module Model.Game.GameNode
    ( GameNode(..)
    ) where

import Data.Aeson
import Data.Text

data GameNode
    = NodePilgrim
    | NodePath
    | NodeGoal
    deriving (Eq, Show)

instance FromJSON GameNode where
    parseJSON = withText "GameNode" $ \v -> return $ case v of
        "NodePilgrim" -> NodePilgrim
        "NodePath" -> NodePath
        "NodeGoal" -> NodeGoal

instance ToJSON GameNode where
    toJSON = String . pack . show