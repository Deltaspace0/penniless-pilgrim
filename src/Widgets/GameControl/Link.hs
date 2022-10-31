module Widgets.GameControl.Link
    ( LinkForm(..)
    , Link(..)
    , linkTransform
    ) where

import Data.Maybe
import Monomer

import qualified Model.Game as G

data LinkForm = LinkBack | LinkForward deriving (Eq, Show)

data Link = Link
    { _linkColor :: Color
    , _linkForm  :: Maybe LinkForm
    } deriving (Eq, Show)

linkTransform :: Maybe G.Link -> Maybe Link
linkTransform Nothing = Nothing
linkTransform (Just G.LinkBack) = Just Link
    { _linkColor = white
    , _linkForm  = Just LinkBack
    }
linkTransform (Just G.LinkForward) = Just Link
    { _linkColor = white
    , _linkForm  = Just LinkForward
    }