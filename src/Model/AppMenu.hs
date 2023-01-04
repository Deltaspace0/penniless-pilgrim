module Model.AppMenu
    ( AppMenu(..)
    ) where

data AppMenu
    = ConfigMenu
    | GameSavesMenu
    deriving (Eq, Show)