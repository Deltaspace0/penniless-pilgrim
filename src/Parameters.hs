{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Parameters
    ( module ConfigSlider
    , AppParameters(..)
    , gridColumnsSlider
    , gridRowsSlider
    , linkToNodeSlider
    , nodeToWidthSlider
    , gameControlWidth
    , gameControlHeight
    , keyConfig
    ) where

import Control.Lens
import Data.Default
import Data.Text (Text)

import ConfigSlider
import Event

data AppParameters = AppParameters
    { _apGridColumnsSlider :: ConfigSlider
    , _apGridRowsSlider    :: ConfigSlider
    , _apLinkToNodeSlider  :: ConfigSlider
    , _apNodeToWidthSlider :: ConfigSlider
    , _apGameControlWidth  :: Double
    , _apGameControlHeight :: Double
    , _apKeyConfig         :: [(Text, AppEvent)]
    } deriving (Eq, Show)

instance Default AppParameters where
    def = AppParameters
        { _apGridColumnsSlider = ConfigSlider 5 2 32 "Columns:"
        , _apGridRowsSlider    = ConfigSlider 5 2 32 "Rows:"
        , _apLinkToNodeSlider  = ConfigSlider 5 3 12
            "Link size to node size ratio:"
        , _apNodeToWidthSlider = ConfigSlider 3 2 12
            "Node size to link width ratio:"
        , _apGameControlWidth  = 400
        , _apGameControlHeight = 500
        , _apKeyConfig =
            [ ("Esc",   ResetPilgrim)
            , ("Up",    MovePilgrim North)
            , ("w",     MovePilgrim North)
            , ("Down",  MovePilgrim South)
            , ("s",     MovePilgrim South)
            , ("Left",  MovePilgrim West)
            , ("a",     MovePilgrim West)
            , ("Right", MovePilgrim East)
            , ("d",     MovePilgrim East)
            ]
        }

makeLensesWith abbreviatedFields 'AppParameters