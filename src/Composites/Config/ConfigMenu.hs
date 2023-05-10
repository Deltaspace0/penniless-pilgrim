module Composites.Config.ConfigMenu
    ( ConfigMenu(..)
    ) where

data ConfigMenu
    = MainMenu
    | ColorMenu
    | NodeColorMenu
    | NodeDefaultColorMenu
    | NodePilgrimColorMenu
    | NodePathColorMenu
    | NodeGoalColorMenu
    | LinkColorMenu
    deriving (Eq, Show)
