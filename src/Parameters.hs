{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Parameters
    ( module ConfigSlider
    , AppParameters(..)
    , gridColumnsSlider
    , gridRowsSlider
    , linkWidthSlider
    , linkSizeSlider
    , nodeSizeSlider
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
    , _apLinkWidthSlider   :: ConfigSlider
    , _apLinkSizeSlider    :: ConfigSlider
    , _apNodeSizeSlider    :: ConfigSlider
    , _apKeyConfig         :: [(Text, AppEvent)]
    } deriving (Eq, Show)

instance Default AppParameters where
    def = AppParameters
        { _apGridColumnsSlider = ConfigSlider 5 2 9     "Columns:"
        , _apGridRowsSlider    = ConfigSlider 5 2 9     "Rows:"
        , _apLinkWidthSlider   = ConfigSlider 4 1 16    "Link width:"
        , _apLinkSizeSlider    = ConfigSlider 64 24 256 "Link size:"
        , _apNodeSizeSlider    = ConfigSlider 12 1 128  "Node size:"
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