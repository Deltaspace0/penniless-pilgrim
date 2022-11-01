{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Widgets.GameControl.Node
    ( Node(..)
    , NodeCfg(..)
    , nodeTransform
    , gameControlNode
    , gameControlNode_
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text)
import Monomer
import Monomer.Widgets.Single
import qualified Monomer.Lens as L

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

data NodeCfg = NodeCfg
    { _defaultColor :: Color
    } deriving (Eq, Show)

instance Default NodeCfg where
    def = NodeCfg
        { _defaultColor = darkGray
        }

makeFields 'Node

nodeTransform :: [G.Node] -> [Node]
nodeTransform = map $ \node -> case node of
    NodePilgrim -> def
        { _nodeColor = Just lightGreen
        }
    NodePath -> def
        { _nodeColor = Just white
        }
    NodeGoal -> def
        { _nodeColor = Just gold
        }

gameControlNode :: [Node] -> WidgetNode s e
gameControlNode nodeStack = gameControlNode_ nodeStack def

gameControlNode_ :: [Node] -> NodeCfg -> WidgetNode s e
gameControlNode_ nodeStack config = gameControlNodeNode where
    gameControlNodeNode = defaultWidgetNode "gameControlNode" widget
    widget = makeGameControlNode nodeStack config

makeGameControlNode :: [Node] -> NodeCfg -> Widget s e
makeGameControlNode nodeStack config = widget where
    widget = createSingle () def
        { singleRender = render
        }

    render wenv node renderer = do
        let style = currentStyle wenv node
            vp = getContentArea node style
        drawEllipse renderer vp $ Just $ if null nodeStack
            then defaultColor
            else fromMaybe defaultColor $ (head nodeStack) ^. color
    
    defaultColor = _defaultColor config