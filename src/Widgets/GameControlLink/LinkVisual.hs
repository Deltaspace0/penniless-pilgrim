module Widgets.GameControlLink.LinkVisual
    ( LinkVisual(..)
    , hlinkTransform
    , vlinkTransform
    ) where

import Monomer

import Widgets.GameControlLink.LinkColorConfig
import Widgets.GameControlLink.LinkForm
import qualified Model.Game as G

data LinkVisual = LinkVisual
    { _linkColor :: Color
    , _linkForm :: LinkForm
    } deriving (Eq, Show)

hlinkTransform
    :: LinkColorConfig
    -> Maybe G.GameLink
    -> Maybe LinkVisual
hlinkTransform config Nothing = Just LinkVisual
    { _linkColor = _lccDefault config
    , _linkForm = LinkDefault
    }
hlinkTransform config (Just G.LinkBack) = Just LinkVisual
    { _linkColor = _lccWest config
    , _linkForm = LinkBack
    }
hlinkTransform config (Just G.LinkForward) = Just LinkVisual
    { _linkColor = _lccEast config
    , _linkForm = LinkForward
    }

vlinkTransform
    :: LinkColorConfig
    -> Maybe G.GameLink
    -> Maybe LinkVisual
vlinkTransform config Nothing = Just LinkVisual
    { _linkColor = _lccDefault config
    , _linkForm = LinkDefault
    }
vlinkTransform config (Just G.LinkBack) = Just LinkVisual
    { _linkColor = _lccNorth config
    , _linkForm = LinkBack
    }
vlinkTransform config (Just G.LinkForward) = Just LinkVisual
    { _linkColor = _lccSouth config
    , _linkForm = LinkForward
    }
