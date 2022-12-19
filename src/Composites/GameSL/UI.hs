{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Composites.GameSL.UI
    ( gameSLComposite
    ) where

import Control.Lens
import Data.Maybe
import Monomer
import TextShow
import qualified Data.Sequence as Seq

import Composites.GameSL.Model
import Model.Game

gameSLComposite
    :: (CompositeEvent sp, CompositeEvent ep)
    => ALens' sp GameSLModel
    -> WidgetNode sp ep
gameSLComposite modelLens = composite' where
    composite' = composite wt modelLens buildUI handleGameSLEvent
    wt = "gameSLComposite"

buildUI :: UIBuilder GameSLModel GameSLEvent
buildUI _ model = widgetTree where
    widgetTree = vstack_ [childSpacing_ 16]
        [ hgrid_ [childSpacing_ 16]
            [ button "New slot" GameSLNewSlot
            , button "Save" GameSLSave `nodeEnabled` selected
            , button "Load" GameSLLoad `nodeEnabled` selected
            , button "Remove" GameSLRemove `nodeEnabled` selected
            ]
        , selectList selectedGame savedGamesList makeRow
        ]
    selected = not $ null $ model ^. selectedGame
    savedGamesList = Just <$> model ^. savedGames
    makeRow gameInfo = label caption where
        GameInfo _ caption = fromJust gameInfo