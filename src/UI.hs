{-# LANGUAGE RecordWildCards #-}

module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import Monomer.SaveManager

import Common.Util
import Composites
import Model
import Widgets.GameControl

buildUI :: UIBuilder AppModel AppEvent
buildUI _ AppModel{..} = widgetTree where
    vstack' = vstack_ [childSpacing_ 16]
    hstack' = hstack_ [childSpacing_ 32]
    boxCenter = box_ [alignCenter]
    widgetTree = hstack'
        [ boxCenter $ gameControlM `nodeKey` "mainGrid"
        , separatorLine
        , side `styleBasic` [sizeReqW $ fixedSize 400]
        ] `styleBasic` [padding 32]
    gameControlM = gameControl $ GameControlData
        { _gcdGameLens = gameSaves . currentData
        , _gcdGetVisualGrid = flip transformGrid _apColorConfig
        , _gcdConfig = mconcat
            [ duration $ fp _apGridAnimationSlider
            , gameLinkToNodeRatio $ fp _apLinkToNodeSlider
            , gameNodeToWidthRatio $ fp _apNodeToWidthSlider
            , gameMinWidth _apGameControlWidth
            , gameMinHeight _apGameControlHeight
            , gameDefaultNodeVisual $ defaultVisual _apColorConfig
            , gameNextScoreField nextTax
            ]
        }
    side = case _appActiveMenu of
        Just ConfigMenu -> vstack' $ sideWidgets <>
            [ separatorLine
            , configComposite_ parameters
                [ configFilePath _appParametersPath
                , onGameChange AppUpdateGameWithConfig
                ]
            ]
        Just GameSavesMenu -> vstack' $ sideWidgets <>
            [ separatorLine
            , saveManager_ gameSaves
                [ onSavesChange AppSaveGamesToFile
                , captionMethod gameSavesCaptionMethod
                ]
            ]
        _ -> boxCenter $ vstack' sideWidgets
    sideWidgets =
        [ bigNumberLabel totalTax "Total tax: "
        , bigNumberLabel _appNextTax "Next tax: "
        , boxCenter $ hstack' $
            [ button "Reset" $ AppSetGame _appInitGame
            , optionButton "Save/load game" gameSavesMenu activeMenu
            , optionButton "Config" configMenu activeMenu
            ] <> [hideButton | not $ null _appActiveMenu]
        ]
    totalTax = Just $ _tax $ _pilgrim $ _appGameSaves ^. currentData
    gameSavesMenu = Just GameSavesMenu
    configMenu = Just ConfigMenu
    hideButton = optionButton "Hide" Nothing activeMenu
    Parameters{..} = _appParameters
    fp = view currentValue

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
    label' = label $ fromMaybe text $ (text <>) . showt' <$> number
