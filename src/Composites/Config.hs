{-# LANGUAGE FlexibleContexts #-}

module Composites.Config
    ( module Composites.Config.ConfigCfg
    , configComposite
    , configComposite_
    ) where

import Control.Lens
import Data.Default
import Monomer

import Composites.Config.ConfigCfg
import Composites.Config.ConfigEvent
import Composites.Config.ConfigModel
import Composites.Config.UI
import Model.Parameters

configComposite
    :: (CompositeEvent sp, CompositeEvent ep)
    => ALens' sp Parameters
    -> WidgetNode sp ep
configComposite field = configComposite_ field def

configComposite_
    :: (CompositeEvent sp, CompositeEvent ep)
    => ALens' sp Parameters
    -> [ConfigCfg ep]
    -> WidgetNode sp ep
configComposite_ field configs = node where
    node = compositeD_ wt wdata' buildUI eventHandler cmpConfigs
    wt = "configComposite"
    wdata' = WidgetValue initConfigModel
    wdata = WidgetLens field
    eventHandler = handleEvent wdata config
    config = mconcat configs
    cmpConfigs = [compositeMergeModel mergeHandler]
    mergeHandler _ pm _ = parameters .~ widgetDataGet pm wdata
