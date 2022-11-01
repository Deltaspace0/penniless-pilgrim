{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControl.Node
    ( Node(..)
    , nodeTransform
    ) where

import Data.Default
import Data.Maybe
import Data.Text (Text)
import Monomer

import Model.Game hiding (Node)
import qualified Model.Game as G

data Node = Node
    { _nodeColor       :: Maybe Color
    , _nodeHoverColor  :: Maybe Color
    , _nodeActiveColor :: Maybe Color
    } deriving (Eq, Show)

instance Default Node where
    def = Node
        { _nodeColor       = Nothing
        , _nodeHoverColor  = Nothing
        , _nodeActiveColor = Nothing
        }

nodeTransform :: [G.Node] -> [Node]
nodeTransform = map $ \node -> case node of
    NodePilgrim -> def {_nodeColor = Just lightGreen}
    NodePath    -> def {_nodeColor = Just white}
    NodeGoal    -> def {_nodeColor = Just gold}