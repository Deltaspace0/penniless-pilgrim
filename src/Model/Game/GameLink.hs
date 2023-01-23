{-# LANGUAGE DeriveGeneric #-}

module Model.Game.GameLink
    ( GameLink(..)
    ) where

import Data.Aeson
import GHC.Generics

data GameLink
    = LinkBack
    | LinkForward
    deriving (Eq, Show, Generic)

instance FromJSON GameLink
instance ToJSON GameLink
