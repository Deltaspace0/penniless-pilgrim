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
    spacingCfg = [childSpacing_ 16]
    crCfg = [onChange (const ResizeGrid :: Double -> AppEvent)]
    centralWidgets =
        [ gameControlM model `nodeKey` "mainGrid"
        , separatorLine `nodeVisible` model ^. showConfig
        , vstack_ spacingCfg
            [ configLabel   model gridColumnsSlider
            , configSlider_ model gridColumnsSlider crCfg
            , configLabel   model gridRowsSlider
            , configSlider_ model gridRowsSlider crCfg
            , configLabel   model linkWidthSlider
            , configSlider  model linkWidthSlider
            , configLabel   model linkSizeSlider
            , configSlider  model linkSizeSlider
            , configLabel   model nodeSizeSlider
            , configSlider  model nodeSizeSlider
            ] `nodeVisible` model ^. showConfig
        ]
    widgetTree = vstack_ spacingCfg
        [ box_ [alignCenter] $ taxLabel model
        , separatorLine
        , box_ [alignCenter] $ hstack_ spacingCfg centralWidgets
        , box_ [alignCenter] $ hstack_ spacingCfg
            [ button "Reset" ResetPilgrim
            , button "Config" ToggleConfig
            ]
        ] `styleBasic` [padding 64]

gameControlM :: AppModel -> WidgetNode AppModel AppEvent
gameControlM model = keystroke kc $ gameControl_ game def
    { _linkWidth = getParameter $ linkWidthSlider . csCurrent
    , _linkSize  = getParameter $ linkSizeSlider . csCurrent
    , _nodeSize  = getParameter $ nodeSizeSlider . csCurrent
    } where
        game = model ^. currentGame
        getParameter f = model ^. parameters . f
        kc = getParameter keyConfig

taxLabel :: AppModel -> WidgetNode AppModel AppEvent
taxLabel model = label t'' `styleBasic` [textSize 32] where
    t = show $ _tax $ _pilgrim $ model ^. currentGame
    (i, m) = break (== '.') t
    t' = if m == ".0" then i else t
    t'' = "Tax total: " <> pack t'

configLabel
    :: AppModel
    -> Lens' AppParameters ConfigSlider
    -> WidgetNode AppModel AppEvent
configLabel model slider = label $ caption <> " " <> t where
    t = showt (floor value :: Int)
    value   = model ^. parameters . slider . csCurrent
    caption = model ^. parameters . slider . csCaption

configSlider
    :: AppModel
    -> Lens' AppParameters ConfigSlider
    -> WidgetNode AppModel AppEvent
configSlider model slider = configSlider_ model slider []

configSlider_
    :: AppModel
    -> Lens' AppParameters ConfigSlider
    -> [SliderCfg AppModel AppEvent Double]
    -> WidgetNode AppModel AppEvent
configSlider_ model slider cfg = hslider_ value a b cfg' where
    value = parameters . slider . csCurrent
    a = model ^. parameters . slider . csMin
    b = model ^. parameters . slider . csMax
    cfg' = [wheelRate 1, dragRate 1] ++ cfg