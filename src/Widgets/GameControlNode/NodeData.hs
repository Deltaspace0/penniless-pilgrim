{-# LANGUAGE RecordWildCards #-}

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
gameControlNodeWidgetType NodeData{..} = WidgetType widgetType where
    widgetType = "gameControlNode" <> showt _ndPosition

baseStyleFromNodeData
    :: (ButtonColors c)
    => NodeData s c
    -> Maybe Style
baseStyleFromNodeData NodeData{..} = Just style where
    style = if _ndClickable
        then basicStyle
            { _styleHover = Just $ def
                { _sstFgColor = Just $ if null _ndVisualStack
                    then getHoverColor _ndDefaultColors
                    else _nodeColorHover $ head _ndVisualStack
                , _sstCursorIcon = Just CursorHand
                }
            , _styleActive = Just $ def
                { _sstFgColor = Just $ if null _ndVisualStack
                    then getActiveColor _ndDefaultColors
                    else _nodeColorActive $ head _ndVisualStack
                , _sstCursorIcon = Just CursorHand
                }
            }
        else basicStyle
    basicStyle = def
        { _styleBasic = Just $ def
            { _sstFgColor = Just $ if null _ndVisualStack
                then getDefaultColor _ndDefaultColors
                else _nodeColorDefault $ head _ndVisualStack
            }
        }
