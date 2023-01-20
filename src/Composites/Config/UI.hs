{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Composites.Config.UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.EnhancedSlider

import Composites.Config.ConfigEvent
import Composites.Config.ConfigModel
import Composites.Config.Parameters
import Util

buildUI :: UIBuilder ConfigModel ConfigEvent
buildUI _ model = widgetTree where
    widgetTree = vstack'
        [ button (model ^. saveCaption) ConfigSave
        , button (model ^. loadCaption) ConfigLoad
        , vscroll_ [wheelRate 20] paddedConfigSliders
        ]
    paddedConfigSliders = configSliders `styleBasic` [paddingR 16]
    configSliders = vstack'
        [ configSlider_ model gridColumnsSlider [reportEvent]
        , configSlider_ model gridRowsSlider [reportEvent]
        , configSlider model gridAnimationSlider
        , configSlider model linkToNodeSlider
        , configSlider model nodeToWidthSlider
        ]
    vstack' = vstack_ [childSpacing_ 16]
    reportEvent = ConfigReportGameChange

configSlider
    :: ConfigModel
    -> Lens' Parameters ConfigSlider
    -> WidgetNode ConfigModel ConfigEvent
configSlider model slider = configSlider_ model slider []

configSlider_
    :: ConfigModel
    -> Lens' Parameters ConfigSlider
    -> [ConfigEvent]
    -> WidgetNode ConfigModel ConfigEvent
configSlider_ model slider events = widget where
    widget = enhancedSlider_ field a b config
    field = parameters . slider . currentValue
    a = _csMin slider'
    b = _csMax slider'
    config =
        [ dragRate $ toRational $ _csChangeRate slider'
        , titleCaption $ _csCaption slider'
        ] <> map transformEvent events'
    events' = saveLoadCaptionEvents <> events
    saveLoadCaptionEvents =
        [ ConfigSetSaveCaption saveConfigCaption'
        , ConfigSetLoadCaption loadConfigCaption'
        ]
    transformEvent = onChange . const'
    const' e = const e :: Double -> ConfigEvent
    saveConfigCaption' = model ^. initialSaveCaption
    loadConfigCaption' = model ^. initialLoadCaption
    slider' = model ^. parameters . slider
