module Widgets.ButtonColors
    ( ButtonColors
    , getDefaultColor
    , getHighlightColor
    , getHoverColor
    , getActiveColor
    ) where

import Monomer

class ButtonColors a where
    getDefaultColor :: a -> Color
    getHighlightColor :: a -> Color
    getHoverColor :: a -> Color
    getActiveColor :: a -> Color
    getHighlightColor = getDefaultColor
    getHoverColor = getDefaultColor
    getActiveColor = getDefaultColor
