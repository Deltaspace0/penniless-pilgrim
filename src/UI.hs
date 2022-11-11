{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Default
import Data.Maybe
import Data.Text (Text, pack)
import Monomer
import TextShow (showt)
import qualified Monomer.Lens as L

import Widgets.GameControl
import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = widgetTree where
    vstack' = vstack_ [childSpacing_ 16]
    hstack' = hstack_ [childSpacing_ 32]
    boxCenter = box_ [alignCenter]
    widgetTree = hstack'
        [ boxCenter $ gameControlM model `nodeKey` "mainGrid"
        , separatorLine
        , side
        ] `styleBasic` [padding 32]
    side = if model ^. showConfig
        then vstack' $ sideWidgets <>
            [ separatorLine
            , button (model ^. saveConfigCaption) AppSaveConfig
            , button (model ^. loadConfigCaption) AppLoadConfig
            , vscroll_ [wheelRate 20] paddedConfigSliders
            ]
        else boxCenter $ vstack' sideWidgets
    sideWidgets =
        [ totalTaxLabel
        , nextTaxLabel
        , boxCenter $ hstack'
            [ button "Reset" AppResetGame
            , toggleButton "Config" showConfig
            ]
        ]
    configSliders = vstack' $ concat
        [ configSlider_ model gridColumnsSlider [AppResizeGrid]
        , configSlider_ model gridRowsSlider [AppResizeGrid]
        , configSlider model gridAnimationSlider
        , configSlider model linkToNodeSlider
        , configSlider model nodeToWidthSlider
        ]
    paddedConfigSliders = configSliders `styleBasic` [paddingR 16]
    totalTaxLabel = bigNumberLabel totalTax "Total tax: "
    nextTaxLabel = bigNumberLabel nextTax' "Next tax: "
    totalTax = Just $ _tax $ _pilgrim $ model ^. currentGame
    nextTax' = model ^. nextTax

gameControlM :: AppModel -> WidgetNode AppModel AppEvent
gameControlM model = gameControl $ GameControlData
    { _gcdGame = currentGame
    , _gcdNextTax = nextTax
    , _gcdColors = get colors
    , _gcdAnimationDuration = get $ gridAnimationSlider . csCurrent
    , _gcdLinkToNodeRatio = get $ linkToNodeSlider  . csCurrent
    , _gcdNodeToWidthRatio = get $ nodeToWidthSlider . csCurrent
    , _gcdWidth = get gameControlWidth
    , _gcdHeight = get gameControlHeight
    } where
        get f = model ^. parameters . f

bigNumberLabel
    :: Maybe Double
    -> Text
    -> WidgetNode AppModel AppEvent
bigNumberLabel number text = styledLabel where
    styledLabel = styleBasic label'
        [ textCenter
        , textSize 24
        , sizeReqW $ expandSize 800 1
        ]
    label' = if null number
        then label text
        else label $ text <> showt' (fromJust number)

configSlider
    :: AppModel
    -> Lens' AppParameters ConfigSlider
    -> [WidgetNode AppModel AppEvent]
configSlider model slider = configSlider_ model slider []

configSlider_
    :: AppModel
    -> Lens' AppParameters ConfigSlider
    -> [AppEvent]
    -> [WidgetNode AppModel AppEvent]
configSlider_ model slider events =
    [ label $ caption <> " " <> showt' current
    , hstack_ [childSpacing_ 32]
        [ hslider_ field a b config
        , button' "-" $ AppSetParameters decreasedParameters
        , button' "+" $ AppSetParameters increasedParameters
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
            [ AppSetSaveConfigCaption saveConfigCaption'
            , AppSetLoadConfigCaption loadConfigCaption'
            ]
        transformEvent e = onChange (const e :: Double -> AppEvent)
        saveConfigCaption' = model ^. initialSaveConfigCaption
        loadConfigCaption' = model ^. initialLoadConfigCaption
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

showt' :: Double -> Text
showt' number = pack result where
    result = if m == ".0" then i else t
    (i, m) = break (== '.') t
    t = show $ (fromIntegral $ round $ number*1000)/1000
