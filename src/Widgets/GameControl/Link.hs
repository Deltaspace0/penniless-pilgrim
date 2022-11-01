module Widgets.GameControl.Link
    ( LinkForm(..)
    , Link(..)
    , hlinkTransform
    , vlinkTransform
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

hlinkTransform :: Colors -> Maybe G.Link -> Maybe Link
hlinkTransform _ Nothing = Nothing
hlinkTransform colors (Just G.LinkBack) = Just Link
    { _linkColor = _linkWest colors
    , _linkForm  = Just LinkBack
    }
hlinkTransform colors (Just G.LinkForward) = Just Link
    { _linkColor = _linkEast colors
    , _linkForm  = Just LinkForward
    }

vlinkTransform :: Colors -> Maybe G.Link -> Maybe Link
vlinkTransform _ Nothing = Nothing
vlinkTransform colors (Just G.LinkBack) = Just Link
    { _linkColor = _linkNorth colors
    , _linkForm  = Just LinkBack
    }
vlinkTransform colors (Just G.LinkForward) = Just Link
    { _linkColor = _linkSouth colors
    , _linkForm  = Just LinkForward
    }