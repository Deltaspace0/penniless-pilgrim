{-# LANGUAGE OverloadedStrings #-}

module Composites.Config
    ( module Composites.Config.Model
    , configComposite
    , configComposite_
    ) where

import Control.Lens
import Monomer

import Composites.Config.Model
import Composites.Config.UI

configComposite
    :: (Eq ep, CompositeEvent sp, CompositeEvent ep)
    => ALens' sp ConfigModel
    -> WidgetNode sp ep
configComposite modelLens = configComposite_ modelLens Nothing

configComposite_
    :: (Eq ep, CompositeEvent sp, CompositeEvent ep)
    => ALens' sp ConfigModel
    -> Maybe ep
    -> WidgetNode sp ep
configComposite_ modelLens onGameChange = composite' where
    composite' = composite wt modelLens buildUI eventHandler
    wt = "configComposite"
    eventHandler = handleConfigEvent onGameChange