{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Composites.Config.UI
    ( buildUI
    ) where

import Control.Lens
import Monomer
import Monomer.EnhancedSlider

import Common.Util
import Composites.Config.ConfigEvent
import Composites.Config.ConfigModel
import Composites.Config.ConfigMenu
import Model.Parameters

buildUI :: UIBuilder ConfigModel ConfigEvent
buildUI _ model@(ConfigModel{..}) = widgetTree where
    widgetTree = zstack
        [ vstack'
            [ hgrid'
                [ button "Save config" ConfigSave
                , button "Load config" ConfigLoad
                ]
            , vscroll_ [wheelRate 20] paddedConfigMenu
            ]
        , widgetMaybe _cfgAlertMessage createAlert
        ]
    paddedConfigMenu = configMenu `styleBasic` [paddingR 16]
    configMenu = vstack' $ case _cfgCurrentMenu of
        MainMenu ->
            [ configSlider_ model gridColumnsSlider [reportEvent]
            , configSlider_ model gridRowsSlider [reportEvent]
            , configSlider model gridAnimationSlider
            , button "Start position" $ ConfigGoto StartPosMenu
            , button "Appearance" $ ConfigGoto AppearanceMenu
            ]
        StartPosMenu ->
            [ separatorLine
            , button "Go back" $ ConfigGoto MainMenu
            , separatorLine
            , label "Change pilgrim start position"
            , configSlider_ model pilgrimStartXSlider [reportEvent]
            , configSlider_ model pilgrimStartYSlider [reportEvent]
            ]
        AppearanceMenu ->
            [ separatorLine
            , button "Go back" $ ConfigGoto MainMenu
            , separatorLine
            , configSlider model linkToNodeSlider
            , configSlider model nodeToWidthSlider
            , separatorLine
            , label "Change colors"
            , hgrid'
                [ button "Node colors" $ ConfigGoto NodeColorMenu
                , button "Link colors" $ ConfigGoto LinkColorMenu
                ]
            ]
        NodeColorMenu ->
            [ separatorLine
            , button "Go back" $ ConfigGoto AppearanceMenu
            , separatorLine
            , label "Change node colors"
            , hgrid'
                [ button "Default" $ ConfigGoto NodeDefaultColorMenu
                , button "Pilgrim" $ ConfigGoto NodePilgrimColorMenu
                ]
            , hgrid'
                [ button "Path" $ ConfigGoto NodePathColorMenu
                , button "Goal" $ ConfigGoto NodeGoalColorMenu
                ]
            ]
        NodeDefaultColorMenu ->
            [ separatorLine
            , button "Go back" $ ConfigGoto NodeColorMenu
            , separatorLine
            , label "Change default node colors"
            , label "Default color:"
            , colorPicker' $ field . nodeDefault
            , label "Hover color:"
            , colorPicker' $ field . nodeHover
            , label "Active color:"
            , colorPicker' $ field . nodeActive
            , label "Highlight color:"
            , colorPicker' $ field . nodeHighlight
            ] where field = nodeField . nccDefault
        NodePilgrimColorMenu ->
            [ separatorLine
            , button "Go back" $ ConfigGoto NodeColorMenu
            , separatorLine
            , label "Change pilgrim node colors"
            , label "Default color:"
            , colorPicker' $ field . nodeDefault
            , label "Hover color:"
            , colorPicker' $ field . nodeHover
            , label "Active color:"
            , colorPicker' $ field . nodeActive
            , label "Highlight color:"
            , colorPicker' $ field . nodeHighlight
            ] where field = nodeField . nccPilgrim
        NodePathColorMenu ->
            [ separatorLine
            , button "Go back" $ ConfigGoto NodeColorMenu
            , separatorLine
            , label "Change path node colors"
            , label "Default color:"
            , colorPicker' $ field . nodeDefault
            , label "Hover color:"
            , colorPicker' $ field . nodeHover
            , label "Active color:"
            , colorPicker' $ field . nodeActive
            , label "Highlight color:"
            , colorPicker' $ field . nodeHighlight
            ] where field = nodeField . nccPath
        NodeGoalColorMenu ->
            [ separatorLine
            , button "Go back" $ ConfigGoto NodeColorMenu
            , separatorLine
            , label "Change goal node colors"
            , label "Default color:"
            , colorPicker' $ field . nodeDefault
            , label "Hover color:"
            , colorPicker' $ field . nodeHover
            , label "Active color:"
            , colorPicker' $ field . nodeActive
            , label "Highlight color:"
            , colorPicker' $ field . nodeHighlight
            ] where field = nodeField . nccGoal
        LinkColorMenu ->
            [ separatorLine
            , button "Go back" $ ConfigGoto AppearanceMenu
            , separatorLine
            , label "Change link colors"
            , label "Default color:"
            , colorPicker' $ linkField . lccDefault
            , label "North color:"
            , colorPicker' $ linkField . lccNorth
            , label "South color:"
            , colorPicker' $ linkField . lccSouth
            , label "West color:"
            , colorPicker' $ linkField . lccWest
            , label "East color:"
            , colorPicker' $ linkField . lccEast
            ]
    colorPicker'
        :: ALens' Parameters Color
        -> WidgetNode ConfigModel ConfigEvent
    colorPicker' field = colorPickerD_ wdata c cmpConfigs where
        wdata = WidgetLens $ parameters . field
        c = [onChange $ ConfigSetParameters . newParameters field]
        cmpConfigs = [mergeRequired $ \_ _ _ -> True]
    newParameters :: ALens' Parameters Color -> Color -> Parameters
    newParameters field v = _cfgParameters & field #~ v
    nodeField = colorConfig . ccGameControlNode
    linkField = colorConfig . ccGameControlLink
    vstack' = vstack_ [childSpacing_ 16]
    hgrid' = hgrid_ [childSpacing_ 16]
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
configSlider_ ConfigModel{..} slider events = widget where
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
    slider' = _cfgParameters ^. slider
    newParameters v = _cfgParameters & slider . currentValue .~ v
