{-# LANGUAGE OverloadedStrings #-}

module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import qualified Monomer.Lens as L

import Composites
import Model
import Util
import Widgets.GameControl

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
            , configComposite configModel
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
    { _gcdGameLens = currentGame
    , _gcdNextTaxLens = nextTax
    , _gcdColors = get colors
    , _gcdAnimationDuration = get $ gridAnimationSlider . csCurrent
    , _gcdLinkToNodeRatio = get $ linkToNodeSlider  . csCurrent
    , _gcdNodeToWidthRatio = get $ nodeToWidthSlider . csCurrent
    , _gcdWidth = get gameControlWidth
    , _gcdHeight = get gameControlHeight
    } where
        get f = model ^. configModel . parameters . f

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