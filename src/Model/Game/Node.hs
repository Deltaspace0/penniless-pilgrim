{-# LANGUAGE OverloadedStrings #-}

module Model.Game.Node
    ( Node(..)
    ) where

import Data.Aeson
import Data.Text

data Node
    = NodePilgrim
    | NodePath
    | NodeGoal
    deriving (Eq, Show)

instance FromJSON Node where
    parseJSON = withText "Node" $ \v -> return $ case v of
        "NodePilgrim" -> NodePilgrim
        "NodePath" -> NodePath
        "NodeGoal" -> NodeGoal

instance ToJSON Node where
    toJSON = String . pack . show