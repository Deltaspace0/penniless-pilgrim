{-# LANGUAGE DeriveGeneric #-}

module Model.Game.GameNode
    ( GameNode(..)
    ) where

import Data.Aeson
import GHC.Generics

data GameNode
    = NodePilgrim
    | NodePath Double
    | NodeGoal
    deriving (Eq, Show, Generic)

instance FromJSON GameNode
instance ToJSON GameNode
