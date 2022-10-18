{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}

module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Default
import Data.Text (pack)
import Monomer
import TextShow (showt)

import GameControl
import Model

buildUI :: UIBuilder AppModel AppEvent
buildUI wenv model = widgetTree where
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
            , configSlider  model linkToNodeSlider
            , configSlider  model nodeToWidthSlider
            ]
        else boxCenter $ vstack' sideWidgets
    sideWidgets =
        [ taxLabel model
        , boxCenter $ hstack'
            [ button "Reset" ResetPilgrim
            , button "Config" ToggleConfig
            ]
        ]

gameControlM :: AppModel -> WidgetNode AppModel AppEvent
gameControlM model = keystroke kc $ gameControl_ game def
    { _gcWidth          = get gameControlWidth
    , _gcHeight         = get gameControlHeight
    , _linkToNodeRatio  = get $ linkToNodeSlider  . csCurrent
    , _nodeToWidthRatio = get $ nodeToWidthSlider . csCurrent
    } where
        game = model ^. currentGame
        get f = model ^. parameters . f
        kc = toKeyStroke $ get keyConfig

taxLabel :: AppModel -> WidgetNode AppModel AppEvent
taxLabel model = label' `styleBasic` styleParameters where
    t = show $ _tax $ _pilgrim $ model ^. currentGame
    t' = if m == ".0" then i else t
    (i, m) = break (== '.') t
    label' = label $ "Tax total: " <> pack t'
    styleParameters =
        [ textCenter
        , textSize 24
        , sizeReqW $ expandSize 800 1
        ]

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