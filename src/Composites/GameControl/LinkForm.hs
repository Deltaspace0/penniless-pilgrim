module Composites.GameControl.LinkForm
    ( LinkForm(..)
    ) where

data LinkForm
    = LinkBack
    | LinkForward
    | LinkDefault
    deriving (Eq, Show)
