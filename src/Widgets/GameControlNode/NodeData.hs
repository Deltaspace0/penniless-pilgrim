module Widgets.GameControlNode.NodeData
    ( NodeData(..)
    , gameControlNodeWidgetType
    , baseStyleFromNodeData
    ) where

import Data.Default
import Monomer
import TextShow

import Widgets.ButtonColors
import Widgets.GameControlNode.NodeVisual

data NodeData s c = NodeData
    { _ndVisualStack :: [NodeVisual]
    , _ndGameControlId :: WidgetId
    , _ndPosition :: (Int, Int)
    , _ndClickable :: Bool
    , _ndNextTax :: Maybe Double
    , _ndNextTaxLens :: WidgetData s (Maybe Double)
    , _ndDefaultColors :: c
    , _ndAnimationDuration :: Double
    }

gameControlNodeWidgetType :: NodeData s c -> WidgetType
gameControlNodeWidgetType nodeData = WidgetType widgetType where
    widgetType = "gameControlNode" <> showt (_ndPosition nodeData)

baseStyleFromNodeData
    :: (ButtonColors c)
    => NodeData s c
    -> Maybe Style
baseStyleFromNodeData nodeData = Just style where
    style = if clickable
        then basicStyle
            { _styleHover = Just $ def
                { _sstFgColor = Just $ if null visualStack
                    then getHoverColor colorConfig
                    else _nodeColorHover $ head visualStack
                , _sstCursorIcon = Just CursorHand
                }
            , _styleActive = Just $ def
                { _sstFgColor = Just $ if null visualStack
                    then getActiveColor colorConfig
                    else _nodeColorActive $ head visualStack
                , _sstCursorIcon = Just CursorHand
                }
            }
        else basicStyle
    clickable = _ndClickable nodeData
    basicStyle = def
        { _styleBasic = Just $ def
            { _sstFgColor = Just $ if null visualStack
                then getDefaultColor colorConfig
                else _nodeColorDefault $ head visualStack
            }
        }
    visualStack = _ndVisualStack nodeData
    colorConfig = _ndDefaultColors nodeData
