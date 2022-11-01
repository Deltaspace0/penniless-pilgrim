{-# LANGUAGE OverloadedStrings #-}

module Model.Parameters.Colors
    ( Colors(..)
    ) where

import Data.Aeson
import Data.Default
import Monomer

data RGB = RGB Int Int Int

instance FromJSON RGB where
    parseJSON = withObject "RGB" $ \v -> RGB
        <$> v .: "r"
        <*> v .: "g"
        <*> v .: "b"

data Colors = Colors
    { _linkDefault        :: Color
    , _linkBack           :: Color
    , _linkForward        :: Color
    , _nodeDefault        :: Color
    , _nodeHover          :: Color
    , _nodeActive         :: Color
    , _nodePilgrimDefault :: Color
    , _nodePilgrimHover   :: Color
    , _nodePilgrimActive  :: Color
    , _nodePathDefault    :: Color
    , _nodePathHover      :: Color
    , _nodePathActive     :: Color
    , _nodeGoalDefault    :: Color
    , _nodeGoalHover      :: Color
    , _nodeGoalActive     :: Color
    } deriving (Eq, Show)

instance Default Colors where
    def = Colors
        { _linkDefault        = rgb 120 120 117
        , _linkBack           = rgb 247 105 70
        , _linkForward        = rgb 247 105 70
        , _nodeDefault        = rgb 120 120 117
        , _nodeHover          = rgb 157 157 149
        , _nodeActive         = rgb 105 104 103
        , _nodePilgrimDefault = rgb 60 247 53
        , _nodePilgrimHover   = rgb 116 248 111
        , _nodePilgrimActive  = rgb 36 181 31
        , _nodePathDefault    = rgb 247 105 70
        , _nodePathHover      = rgb 245 132 105
        , _nodePathActive     = rgb 185 75 48
        , _nodeGoalDefault    = rgb 221 230 58
        , _nodeGoalHover      = rgb 230 238 98
        , _nodeGoalActive     = rgb 162 169 42
        }

instance FromJSON Colors where
    parseJSON = withObject "Colors" func where
        func v = Colors
            <$> f "link_default"
            <*> f "link_back"
            <*> f "link_forward"
            <*> f "node_default"
            <*> f "node_hover"
            <*> f "node_active"
            <*> f "node_pilgrim_default"
            <*> f "node_pilgrim_hover"
            <*> f "node_pilgrim_active"
            <*> f "node_path_default"
            <*> f "node_path_hover"
            <*> f "node_path_active"
            <*> f "node_goal_default"
            <*> f "node_goal_hover"
            <*> f "node_goal_active" where
                f t = fromRGB <$> v .: t

fromRGB :: RGB -> Color
fromRGB (RGB r g b) = rgb r g b