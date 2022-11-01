module Widgets.GameControl.Link
    ( LinkForm(..)
    , Link(..)
    , linkTransform
    ) where

import Data.Maybe
import Monomer

import Model.Parameters.Colors
import qualified Model.Game as G

data LinkForm = LinkBack | LinkForward deriving (Eq, Show)

data Link = Link
    { _linkColor :: Color
    , _linkForm  :: Maybe LinkForm
    } deriving (Eq, Show)

linkTransform :: Colors -> Maybe G.Link -> Maybe Link
linkTransform _ Nothing = Nothing
linkTransform colors (Just G.LinkBack) = Just Link
    { _linkColor = _linkBack colors
    , _linkForm  = Just LinkBack
    }
linkTransform colors (Just G.LinkForward) = Just Link
    { _linkColor = _linkBack colors
    , _linkForm  = Just LinkForward
    }