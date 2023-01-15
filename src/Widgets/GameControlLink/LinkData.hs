{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControlLink.LinkData
    ( LinkData(..)
    , gameControlHlinkWidgetType
    , gameControlVlinkWidgetType
    ) where

import Monomer
import TextShow

import Widgets.GameControlLink.LinkColorConfig
import Widgets.GameControlLink.LinkVisual

data LinkData = LinkData
    { _ldLink :: Maybe LinkVisual
    , _ldPosition :: (Int, Int)
    , _ldColorConfig :: LinkColorConfig
    , _ldAnimationDuration :: Double
    , _ldNodeToWidthRatio :: Double
    } deriving (Eq, Show)

gameControlHlinkWidgetType :: LinkData -> WidgetType
gameControlHlinkWidgetType linkData = WidgetType widgetType where
    widgetType = "gameControlHlink" <> showt (_ldPosition linkData)

gameControlVlinkWidgetType :: LinkData -> WidgetType
gameControlVlinkWidgetType linkData = WidgetType widgetType where
    widgetType = "gameControlVlink" <> showt (_ldPosition linkData)
