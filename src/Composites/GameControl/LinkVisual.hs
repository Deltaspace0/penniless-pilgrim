module Composites.GameControl.LinkVisual
    ( module Composites.GameControl.LinkForm
    , LinkVisual(..)
    ) where

import Monomer

import Composites.GameControl.LinkForm

data LinkVisual = LinkVisual
    { _linkColor :: Color
    , _linkForm :: LinkForm
    } deriving (Eq, Show)
