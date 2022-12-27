{-# LANGUAGE OverloadedStrings #-}

module Widgets.GameControlLink.LinkData
    ( LinkData(..)
    , gameControlHlinkWidgetType
    , gameControlVlinkWidgetType
    , getLinkColorConfig
    , getAnimationDuration
    ) where

import Monomer
import TextShow

import Widgets.GameControl.GameControlConfig
import Widgets.GameControlLink.LinkVisual

data LinkData = LinkData
    { _ldLink :: Maybe LinkVisual
    , _ldPosition :: (Int, Int)
    , _ldConfig :: GameControlConfig
    } deriving (Eq, Show)

gameControlHlinkWidgetType :: LinkData -> WidgetType
gameControlHlinkWidgetType linkData = WidgetType widgetType where
    widgetType = "gameControlHlink" <> showt (_ldPosition linkData)

gameControlVlinkWidgetType :: LinkData -> WidgetType
gameControlVlinkWidgetType linkData = WidgetType widgetType where
    widgetType = "gameControlVlink" <> showt (_ldPosition linkData)

getLinkColorConfig :: LinkData -> LinkColorConfig
getLinkColorConfig linkData = _gcccLink config where
    config = _gccColorConfig $ _ldConfig linkData

getAnimationDuration :: LinkData -> Double
getAnimationDuration linkData = _gccAnimationDuration config where
    config = _ldConfig linkData