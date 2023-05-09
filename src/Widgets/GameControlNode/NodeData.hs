{-# LANGUAGE RecordWildCards #-}

module Widgets.GameControlNode.NodeData
    ( NodeData(..)
    , gameControlNodeWidgetType
    , baseStyleFromNodeData
    ) where

import Data.Default
import Monomer
import TextShow

import Widgets.GameControlNode.NodeVisual

data NodeData s = NodeData
    { _ndVisualStack :: [NodeVisual]
    , _ndGameControlId :: WidgetId
    , _ndPosition :: (Int, Int)
    , _ndClickable :: Bool
    , _ndNextTax :: Maybe Double
    , _ndNextTaxLens :: WidgetData s (Maybe Double)
    , _ndDefaultVisual :: NodeVisual
    , _ndAnimationDuration :: Double
    }

gameControlNodeWidgetType :: NodeData s -> WidgetType
gameControlNodeWidgetType NodeData{..} = WidgetType widgetType where
    widgetType = "gameControlNode" <> showt _ndPosition

baseStyleFromNodeData :: NodeData s -> Maybe Style
baseStyleFromNodeData NodeData{..} = Just style where
    style = if _ndClickable
        then basicStyle
            { _styleHover = Just $ def
                { _sstFgColor = Just _nodeColorHover
                , _sstCursorIcon = Just CursorHand
                }
            , _styleActive = Just $ def
                { _sstFgColor = Just _nodeColorActive
                , _sstCursorIcon = Just CursorHand
                }
            }
        else basicStyle
    basicStyle = def
        { _styleBasic = Just $ def
            { _sstFgColor = Just _nodeColorDefault
            }
        }
    NodeVisual{..} = if null _ndVisualStack
        then _ndDefaultVisual
        else head _ndVisualStack
