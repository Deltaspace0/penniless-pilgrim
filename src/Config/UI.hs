{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.UI
    ( configComposite
    ) where

import Control.Lens
import Monomer

import Config.Model
import Model.Parameters
import Util

configComposite
    :: (Eq ep, CompositeEvent sp, CompositeEvent ep)
    => ALens' sp (ConfigModel ep)
    -> WidgetNode sp ep
configComposite modelLens = composite' where
    composite' = composite wt modelLens buildUI handleEvent
    wt = "configComposite"

buildUI :: UIBuilder (ConfigModel ep) ConfigEvent
buildUI _ model = widgetTree where
    widgetTree = vstack'
        [ button (model ^. saveCaption) ConfigSave
        , button (model ^. loadCaption) ConfigLoad
        , vscroll_ [wheelRate 20] paddedConfigSliders
        ]
    paddedConfigSliders = configSliders `styleBasic` [paddingR 16]
    configSliders = vstack' $ concat
        [ configSlider_ model gridColumnsSlider [ConfigReportGDC]
        , configSlider_ model gridRowsSlider [ConfigReportGDC]
        , configSlider model gridAnimationSlider
        , configSlider model linkToNodeSlider
        , configSlider model nodeToWidthSlider
        ]
    vstack' = vstack_ [childSpacing_ 16]

configSlider
    :: ConfigModel ep
    -> Lens' AppParameters ConfigSlider
    -> [WidgetNode (ConfigModel ep) ConfigEvent]
configSlider model slider = configSlider_ model slider []

configSlider_
    :: ConfigModel ep
    -> Lens' AppParameters ConfigSlider
    -> [ConfigEvent]
    -> [WidgetNode (ConfigModel ep) ConfigEvent]
configSlider_ model slider events =
    [ label $ caption <> " " <> showt' current
    , hstack_ [childSpacing_ 32]
        [ hslider_ field a b config
        , button' "-" $ ConfigSetParameters decreasedParameters
        , button' "+" $ ConfigSetParameters increasedParameters
        ]
    ] where
        current = slider' ^. csCurrent
        a = slider' ^. csMin
        b = slider' ^. csMax
        changeRate = slider' ^. csChangeRate
        caption = slider' ^. csCaption
        field = parameters . slider . csCurrent
        config =
            [ wheelRate 0
            , dragRate $ toRational changeRate
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
        slider' = p ^. slider
        p = model ^. parameters
        decreasedParameters = p & slider .~ decreasedSlider
        increasedParameters = p & slider .~ increasedSlider
        decreasedSlider = slider' & csCurrent %~ decrease
        increasedSlider = slider' & csCurrent %~ increase
        decrease c = max a $ c-changeRate
        increase c = min b $ c+changeRate
        button' c e = button_ c e buttonConfig `styleBasic`
            [ width 32
            , height 24
            ]
        buttonConfig = map onClick events'