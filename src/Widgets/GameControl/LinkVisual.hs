module Widgets.GameControl.LinkVisual
    ( module Widgets.GameControl.LinkForm
    , LinkVisual(..)
    ) where

import Monomer

import Widgets.GameControl.LinkForm

data LinkVisual = LinkVisual
    { _linkColor :: Color
    , _linkForm :: LinkForm
    } deriving (Eq, Show)
