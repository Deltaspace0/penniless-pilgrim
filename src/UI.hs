module UI
    ( buildUI
    ) where

import Control.Lens
import Data.Maybe
import Data.Text (Text)
import Monomer
import Monomer.SaveManager

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
        [ boxCenter $ gameControlM `nodeKey` "mainGrid"
        , separatorLine
        , side
        ] `styleBasic` [padding 32]
    gameControlM = gameControl $ GameControlData
        { _gcdGameLens = gameSaves . currentData
        , _gcdNextTaxLens = Just nextTax
        , _gcdConfig = model ^. configModel . parameters
        }
    side = case model ^. activeMenu of
        Just ConfigMenu -> vstack' $ sideWidgets <>
            [ separatorLine
            , configComposite_ configModel onGameChange
            ] where onGameChange = Just AppUpdateGameWithConfig
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
        , bigNumberLabel (model ^. nextTax) "Next tax: "
        , boxCenter $ hstack' $
            [ button "Reset" $ AppSetGame $ model ^. initGame
            , optionButton "Save/load game" gameSavesMenu activeMenu
            , optionButton "Config" configMenu activeMenu
            ] <> [hideButton | not $ null $ model ^. activeMenu]
        ]
    totalTax = Just $ _tax $ _pilgrim currentGame
    currentGame = model ^. gameSaves . currentData
    gameSavesMenu = Just GameSavesMenu
    configMenu = Just ConfigMenu
    hideButton = optionButton "Hide" Nothing activeMenu

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
