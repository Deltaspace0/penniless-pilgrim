{-# LANGUAGE RankNTypes #-}

module Composites.Config.UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.EnhancedSlider

import Common.Util
import Composites.Config.ConfigEvent
import Composites.Config.ConfigModel
import Model.Parameters

buildUI :: UIBuilder ConfigModel ConfigEvent
buildUI _ model = widgetTree where
    widgetTree = zstack
        [ vstack'
            [ button "Save config to file" ConfigSave
            , button "Load config from file" ConfigLoad
            , vscroll_ [wheelRate 20] paddedConfigSliders
            ]
        , widgetMaybe (model ^. alertMessage) createAlert
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
    createAlert = (flip alertMsg) $ ConfigSetMessage Nothing

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
    widget = enhancedSliderD_ wdata a b config
        [mergeRequired $ \_ _ _ -> True]
    wdata = WidgetLens $ parameters . slider . currentValue
    a = _csMin slider'
    b = _csMax slider'
    config =
        [ dragRate $ toRational $ _csChangeRate slider'
        , titleMethod $ (((_csCaption slider') <> ": ") <>) . showt'
        , onChange $ ConfigSetParameters . newParameters
        ] <> map transformEvent events
    transformEvent = onChange . const'
    const' e = const e :: Double -> ConfigEvent
    slider' = parameters' ^. slider
    parameters' = model ^. parameters
    newParameters v = parameters' & slider . currentValue .~ v
