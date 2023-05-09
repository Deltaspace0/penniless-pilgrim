module Widgets.GameControlLink.LinkVisual
    ( module Widgets.GameControlLink.LinkForm
    , LinkVisual(..)
    ) where

import Monomer

import Widgets.GameControlLink.LinkForm

data LinkVisual = LinkVisual
    { _linkColor :: Color
    , _linkForm :: LinkForm
    } deriving (Eq, Show)
