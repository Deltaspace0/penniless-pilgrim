{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Parameters.LinkColorConfig
    ( LinkColorConfig(..)
    , lccDefault
    , lccNorth
    , lccSouth
    , lccWest
    , lccEast
    , hlinkTransform
    , vlinkTransform
    ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Default
import Monomer

import Model.Game
import Model.Parameters.RGB
import Widgets.GameControlLink hiding
    ( LinkDefault
    , LinkBack
    , LinkForward
    )
import qualified Widgets.GameControlLink as C

data LinkColorConfig = LinkColorConfig
    { _lccDefault :: Color
    , _lccNorth :: Color
    , _lccSouth :: Color
    , _lccWest :: Color
    , _lccEast :: Color
    } deriving (Eq, Show)

makeLenses 'LinkColorConfig

instance Default LinkColorConfig where
    def = LinkColorConfig
        { _lccDefault = rgb 120 120 117
        , _lccNorth = rgb 247 105 70
        , _lccSouth = rgb 247 105 70
        , _lccWest = rgb 247 105 70
        , _lccEast = rgb 247 105 70
        }

instance FromJSON LinkColorConfig where
    parseJSON = withObject "LinkColorConfig" f where
        f v = LinkColorConfig
            <$> (g "default") .!= _lccDefault def
            <*> (g "north") .!= _lccNorth def
            <*> (g "south") .!= _lccSouth def
            <*> (g "west") .!= _lccWest def
            <*> (g "east") .!= _lccEast def where
                g t = (fromRGB <$>) <$> v .:? t

instance ToJSON LinkColorConfig where
    toJSON LinkColorConfig{..} = object
        [ "default" .= toRGB _lccDefault
        , "north" .= toRGB _lccNorth
        , "south" .= toRGB _lccSouth
        , "west" .= toRGB _lccWest
        , "east" .= toRGB _lccEast
        ]

hlinkTransform
    :: LinkColorConfig
    -> Maybe GameLink
    -> Maybe LinkVisual
hlinkTransform config Nothing = Just LinkVisual
    { _linkColor = _lccDefault config
    , _linkForm = C.LinkDefault
    }
hlinkTransform config (Just LinkBack) = Just LinkVisual
    { _linkColor = _lccWest config
    , _linkForm = C.LinkBack
    }
hlinkTransform config (Just LinkForward) = Just LinkVisual
    { _linkColor = _lccEast config
    , _linkForm = C.LinkForward
    }

vlinkTransform
    :: LinkColorConfig
    -> Maybe GameLink
    -> Maybe LinkVisual
vlinkTransform config Nothing = Just LinkVisual
    { _linkColor = _lccDefault config
    , _linkForm = C.LinkDefault
    }
vlinkTransform config (Just LinkBack) = Just LinkVisual
    { _linkColor = _lccNorth config
    , _linkForm = C.LinkBack
    }
vlinkTransform config (Just LinkForward) = Just LinkVisual
    { _linkColor = _lccSouth config
    , _linkForm = C.LinkForward
    }
