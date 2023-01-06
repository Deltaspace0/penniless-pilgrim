module Widgets.GameControlLink.LinkVisual
    ( LinkVisual(..)
    , hlinkTransform
    , vlinkTransform
    ) where

import Monomer

import Model.Game
import Widgets.GameControlLink.LinkColorConfig

data LinkVisual = LinkVisual
    { _linkColor :: Color
    , _linkForm :: Maybe GameLink
    } deriving (Eq, Show)

hlinkTransform
    :: LinkColorConfig
    -> Maybe GameLink
    -> Maybe LinkVisual
hlinkTransform _ Nothing = Nothing
hlinkTransform config (Just LinkBack) = Just LinkVisual
    { _linkColor = _lccWest config
    , _linkForm = Just LinkBack
    }
hlinkTransform config (Just LinkForward) = Just LinkVisual
    { _linkColor = _lccEast config
    , _linkForm = Just LinkForward
    }

vlinkTransform
    :: LinkColorConfig
    -> Maybe GameLink
    -> Maybe LinkVisual
vlinkTransform _ Nothing = Nothing
vlinkTransform config (Just LinkBack) = Just LinkVisual
    { _linkColor = _lccNorth config
    , _linkForm = Just LinkBack
    }
vlinkTransform config (Just LinkForward) = Just LinkVisual
    { _linkColor = _lccSouth config
    , _linkForm = Just LinkForward
    }
