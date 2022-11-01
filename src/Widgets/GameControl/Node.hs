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
    { _defaultColor       :: Color
    , _defaultHoverColor  :: Maybe Color
    , _defaultActiveColor :: Maybe Color
    } deriving (Eq, Show)

makeFields 'Node

nodeTransform :: [G.Node] -> [Node]
nodeTransform = map $ \node -> case node of
    NodePilgrim -> def
        { _nodeColor       = Just $ rgb 60 247 53
        , _nodeHoverColor  = Just $ rgb 116 248 111
        , _nodeActiveColor = Just $ rgb 36 181 31
        }
    NodePath -> def
        { _nodeColor       = Just $ rgb 247 105 70
        , _nodeHoverColor  = Just $ rgb 245 132 105
        , _nodeActiveColor = Just $ rgb 185 75 48
        }
    NodeGoal -> def
        { _nodeColor       = Just $ rgb 221 230 58
        , _nodeHoverColor  = Just $ rgb 230 238 98
        , _nodeActiveColor = Just $ rgb 162 169 42
        }

gameControlNode :: [Node] -> NodeCfg -> WidgetNode s e
gameControlNode nodeStack config = gameControlNodeNode where
    gameControlNodeNode = defaultWidgetNode "gameControlNode" widget
    widget = makeGameControlNode nodeStack config

makeGameControlNode :: [Node] -> NodeCfg -> Widget s e
makeGameControlNode nodeStack config = widget where
    widget = createSingle () def
        { singleGetBaseStyle    = getBaseStyle
        , singleGetCurrentStyle = getCurrentStyle
        , singleRender          = render
        }

    getBaseStyle _ _ = Just $ def
        { _styleBasic = Just $ def
            { _sstFgColor = Just basicColor
            }
        , _styleHover = Just $ def
            { _sstFgColor = Just hoverColor'
            }
        , _styleActive = Just $ def
            { _sstFgColor = Just activeColor'
            }
        }

    getCurrentStyle wenv node = style where
        style' = currentStyle wenv node
        vp = getContentArea node style'
        style = currentStyle_ c wenv node
        c = def & L.isHovered .~ isNodeHoveredEllipse_ vp

    render wenv node renderer = do
        let style = getCurrentStyle wenv node
            vp = getContentArea node style
        drawEllipse renderer vp $ _sstFgColor style

    defaultColor = _defaultColor config
    basicColor = if null nodeStack
        then defaultColor
        else fromMaybe defaultColor $ nodeHead ^. color
    hoverColor' = fromMaybe basicColor $ if null nodeStack
        then _defaultHoverColor config
        else nodeHead ^. hoverColor
    activeColor' = fromMaybe basicColor $ if null nodeStack
        then _defaultActiveColor config
        else nodeHead ^. activeColor
    nodeHead = head nodeStack