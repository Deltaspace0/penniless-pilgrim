{-# LANGUAGE OverloadedStrings #-}

module Composites.Config
    ( module Composites.Config.Model
    , configComposite
    ) where

import Control.Lens
import Monomer

import Composites.Config.Model
import Composites.Config.UI

configComposite
    :: (Eq ep, CompositeEvent sp, CompositeEvent ep)
    => ALens' sp ConfigModel
    -> WidgetNode sp ep
configComposite modelLens = composite' where
    composite' = composite wt modelLens buildUI handleConfigEvent
    wt = "configComposite"