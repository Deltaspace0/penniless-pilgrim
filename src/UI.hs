{-# LANGUAGE RankNTypes        #-}
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
    crCfg = [onChange (const ResizeGrid :: Double -> AppEvent)]
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
            , configSlider model linkToNodeSlider
            , configSlider model nodeToWidthSlider
            ]
        else boxCenter $ vstack' sideWidgets
    sideWidgets =
        [ taxTotalLabel
        , nextTaxLabel
        , boxCenter $ hstack'
            [ button "Reset" ResetPilgrim
            , button "Config" ToggleConfig
            , button "Save config" SaveConfig
            ]
        ]
    taxTotalLabel = bigNumberLabel taxTotal "Tax total: "
    nextTaxLabel = bigNumberLabel nextTax' "Next tax: "
    taxTotal = Just $ _tax $ _pilgrim $ model ^. currentGame
    nextTax' = model ^. nextTax

gameControlM :: AppModel -> WidgetNode AppModel AppEvent
gameControlM model = gameControl $ GameControlData
    { _gcdGame = currentGame
    , _gcdNextTax = nextTax
    , _gcdColors = get colors
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
        else label $ text <> pack t'
    t = show $ fromJust number
    t' = if m == ".0" then i else t
    (i, m) = break (== '.') t

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
configSlider_ model slider cfg =
    [ label $ caption <> " " <> t
    , hslider_ field a b cfg'
    ] where
        caption = model ^. parameters . slider . csCaption
        t = showt (floor current :: Int)
        field = parameters . slider . csCurrent
        a = model ^. parameters . slider . csMin
        b = model ^. parameters . slider . csMax
        current = model ^. parameters . slider . csCurrent
        cfg' = [wheelRate 1, dragRate 1] <> cfg