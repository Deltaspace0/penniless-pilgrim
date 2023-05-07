module Composites.Config.ConfigCfg
    ( ConfigCfg(..)
    , configFilePath
    , onGameChange
    ) where

import Control.Applicative ((<|>))
import Data.Default

data ConfigCfg ep = ConfigCfg
    { _ccFilePath :: Maybe String
    , _ccGameChange :: Maybe ep
    }

instance Default (ConfigCfg ep) where
    def = ConfigCfg
        { _ccFilePath = Nothing
        , _ccGameChange = Nothing
        }

instance Semigroup (ConfigCfg ep) where
    (<>) a1 a2 = def
        { _ccFilePath = _ccFilePath a2 <|> _ccFilePath a1
        , _ccGameChange = _ccGameChange a2 <|> _ccGameChange a1
        }

instance Monoid (ConfigCfg ep) where
    mempty = def

configFilePath :: Maybe String -> ConfigCfg ep
configFilePath path = def
    { _ccFilePath = path
    }

onGameChange :: ep -> ConfigCfg ep
onGameChange event = def
    { _ccGameChange = Just event
    }
