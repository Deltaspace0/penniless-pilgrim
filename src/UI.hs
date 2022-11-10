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

import Widgets.GameControl
import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI _ model = widgetTree where
    vstack' = vstack_ [childSpacing_ 16]
    hstack' = hstack_ [childSpacing_ 32]
    boxCenter = box_ [alignCenter]
    crCfg = [onChange (const AppResizeGrid :: Double -> AppEvent)]
    widgetTree = hstack'
        [ boxCenter $ gameControlM model `nodeKey` "mainGrid"
        , separatorLine
        , side
        ] `styleBasic` [padding 32]
    side = if model ^. showConfig
        then vstack' $ concat
            [ sideWidgets
            , [separatorLine]
            , configSlider_ model gridColumnsSlider crCfg
            , configSlider_ model gridRowsSlider crCfg
            , configSlider model gridAnimationSlider
            , configSlider model linkToNodeSlider
            , configSlider model nodeToWidthSlider
            , [button (model ^. saveConfigCaption) AppSaveConfig]
            , [button (model ^. loadConfigCaption) AppLoadConfig]
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
    -> [SliderCfg AppModel AppEvent Double]
    -> [WidgetNode AppModel AppEvent]
configSlider_ model slider config =
    [ label $ caption <> " " <> showt' current
    , hslider_ field a b config'
    ] where
        current = slider' ^. csCurrent
        a = slider' ^. csMin
        b = slider' ^. csMax
        changeRate = toRational $ slider' ^. csChangeRate
        caption = slider' ^. csCaption
        field = parameters . slider . csCurrent
        config' =
            [ wheelRate changeRate
            , dragRate changeRate
            , onChange (const eventSave :: Double -> AppEvent)
            , onChange (const eventLoad :: Double -> AppEvent)
            ] <> config
        eventSave = AppSetSaveConfigCaption saveConfigCaption'
        eventLoad = AppSetLoadConfigCaption loadConfigCaption'
        saveConfigCaption' = model ^. initialSaveConfigCaption
        loadConfigCaption' = model ^. initialLoadConfigCaption
        slider' = model ^. parameters . slider

showt' :: Double -> Text
showt' number = pack result where
    result = if m == ".0" then i else t
    (i, m) = break (== '.') t
    t = show $ (fromIntegral $ round $ number*1000)/1000