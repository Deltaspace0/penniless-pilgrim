{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControlNode.NodeData
    ( NodeData(..)
    , gameControlNodeWidgetType
    , getDefaultNodeColors
    , baseStyleFromNodeData
    ) where

import Data.Default
import Monomer
import TextShow

import Widgets.GameControlNode.NodeColorConfig
import Widgets.GameControlNode.NodeVisual

data NodeData s = NodeData
    { _ndVisualStack :: [NodeVisual]
    , _ndGameControlId :: WidgetId
    , _ndPosition :: (Int, Int)
    , _ndClickable :: Bool
    , _ndNextTax :: Maybe Double
    , _ndNextTaxLens :: WidgetData s (Maybe Double)
    , _ndColorConfig :: NodeColorConfig
    , _ndAnimationDuration :: Double
    }

gameControlNodeWidgetType :: NodeData s -> WidgetType
gameControlNodeWidgetType nodeData = WidgetType widgetType where
    widgetType = "gameControlNode" <> showt (_ndPosition nodeData)

getDefaultNodeColors :: NodeData s -> NodeColors
getDefaultNodeColors = _nccDefault . _ndColorConfig

baseStyleFromNodeData :: NodeData s -> Maybe Style
baseStyleFromNodeData nodeData = Just style where
    style = if clickable
        then basicStyle
            { _styleHover = Just $ def
                { _sstFgColor = Just $ if null visualStack
                    then _nodeHover colorConfig
                    else _nodeColorHover $ head visualStack
                , _sstCursorIcon = Just CursorHand
                }
            , _styleActive = Just $ def
                { _sstFgColor = Just $ if null visualStack
                    then _nodeActive colorConfig
                    else _nodeColorActive $ head visualStack
                , _sstCursorIcon = Just CursorHand
                }
            }
        else basicStyle
    clickable = _ndClickable nodeData
    basicStyle = def
        { _styleBasic = Just $ def
            { _sstFgColor = Just $ if null visualStack
                then _nodeDefault colorConfig
                else _nodeColorDefault $ head visualStack
            }
        }
    visualStack = _ndVisualStack nodeData
    colorConfig = getDefaultNodeColors nodeData
