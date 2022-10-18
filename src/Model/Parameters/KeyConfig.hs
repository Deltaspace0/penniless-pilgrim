{-# LANGUAGE OverloadedStrings #-}

module Model.Parameters.KeyConfig
    ( KeyConfig(..)
    , toKeyStroke
    ) where

import Data.Aeson
import Data.Default
import Data.Text (Text)

import Model.Event

data KeyConfig = KeyConfig
    { _kcResetPilgrim     :: [Text]
    , _kcMovePilgrimNorth :: [Text]
    , _kcMovePilgrimSouth :: [Text]
    , _kcMovePilgrimWest  :: [Text]
    , _kcMovePilgrimEast  :: [Text]
    } deriving (Eq, Show)

instance Default KeyConfig where
    def = KeyConfig
        { _kcResetPilgrim     = ["Esc"]
        , _kcMovePilgrimNorth = ["Up", "w"]
        , _kcMovePilgrimSouth = ["Down", "s"]
        , _kcMovePilgrimWest  = ["Left", "a"]
        , _kcMovePilgrimEast  = ["Right", "d"]
        }

instance FromJSON KeyConfig where
    parseJSON = withObject "KeyConfig" $ \v -> KeyConfig
        <$> v .: "reset_pilgrim"
        <*> v .: "move_pilgrim_north"
        <*> v .: "move_pilgrim_south"
        <*> v .: "move_pilgrim_west"
        <*> v .: "move_pilgrim_east"

toKeyStroke :: KeyConfig -> [(Text, AppEvent)]
toKeyStroke keyConfig = concat
    [ map (\t -> (t, ResetPilgrim))      resetPilgrim
    , map (\t -> (t, MovePilgrim North)) movePilgrimNorth
    , map (\t -> (t, MovePilgrim South)) movePilgrimSouth
    , map (\t -> (t, MovePilgrim West))  movePilgrimWest
    , map (\t -> (t, MovePilgrim East))  movePilgrimEast
    ] where
        resetPilgrim     = _kcResetPilgrim     keyConfig
        movePilgrimNorth = _kcMovePilgrimNorth keyConfig
        movePilgrimSouth = _kcMovePilgrimSouth keyConfig
        movePilgrimWest  = _kcMovePilgrimWest  keyConfig
        movePilgrimEast  = _kcMovePilgrimEast  keyConfig