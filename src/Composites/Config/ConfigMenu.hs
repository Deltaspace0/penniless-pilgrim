module Composites.Config.ConfigMenu
    ( ConfigMenu(..)
    ) where

data ConfigMenu
    = MainMenu
    | StartPosMenu
    | ColorMenu
    | NodeColorMenu
    | NodeDefaultColorMenu
    | NodePilgrimColorMenu
    | NodePathColorMenu
    | NodeGoalColorMenu
    | LinkColorMenu
    deriving (Eq, Show)
