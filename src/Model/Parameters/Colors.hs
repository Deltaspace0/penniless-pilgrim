{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Parameters.Colors
    ( Colors(..)
    ) where

import Control.Lens ((^.))
import Data.Aeson
import Data.Default
import Monomer
import qualified Monomer.Lens as L

data RGB = RGB Int Int Int

instance FromJSON RGB where
    parseJSON = withObject "RGB" $ \v -> RGB
        <$> v .: "r"
        <*> v .: "g"
        <*> v .: "b"

instance ToJSON RGB where
    toJSON (RGB r g b) = object
        [ "r" .= r
        , "g" .= g
        , "b" .= b
        ]

data Colors = Colors
    { _linkDefault :: Color
    , _linkNorth :: Color
    , _linkSouth :: Color
    , _linkWest :: Color
    , _linkEast :: Color
    , _nodeHighlight :: Color
    , _nodeDefault :: Color
    , _nodeHover :: Color
    , _nodeActive :: Color
    , _nodePilgrimDefault :: Color
    , _nodePilgrimHover :: Color
    , _nodePilgrimActive :: Color
    , _nodePathDefault :: Color
    , _nodePathHover :: Color
    , _nodePathActive :: Color
    , _nodeGoalDefault :: Color
    , _nodeGoalHover :: Color
    , _nodeGoalActive :: Color
    } deriving (Eq, Show)

instance Default Colors where
    def = Colors
        { _linkDefault = rgb 120 120 117
        , _linkNorth = rgb 247 105 70
        , _linkSouth = rgb 247 105 70
        , _linkWest = rgb 247 105 70
        , _linkEast = rgb 247 105 70
        , _nodeHighlight = rgb 234 232 233
        , _nodeDefault = rgb 120 120 117
        , _nodeHover = rgb 157 157 149
        , _nodeActive = rgb 105 104 103
        , _nodePilgrimDefault = rgb 60 247 53
        , _nodePilgrimHover = rgb 116 248 111
        , _nodePilgrimActive = rgb 36 181 31
        , _nodePathDefault = rgb 247 105 70
        , _nodePathHover = rgb 245 132 105
        , _nodePathActive = rgb 185 75 48
        , _nodeGoalDefault = rgb 221 230 58
        , _nodeGoalHover = rgb 230 238 98
        , _nodeGoalActive = rgb 162 169 42
        }

instance FromJSON Colors where
    parseJSON = withObject "Colors" func where
        func v = Colors
            <$> f "link_default"
            <*> f "link_north"
            <*> f "link_south"
            <*> f "link_west"
            <*> f "link_east"
            <*> f "node_highlight"
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
                fromRGB (RGB r g b) = rgb r g b

instance ToJSON Colors where
    toJSON colors = object
        [ "link_default" .= f (_linkDefault colors)
        , "link_north" .= f (_linkNorth colors)
        , "link_south" .= f (_linkSouth colors)
        , "link_west" .= f (_linkWest colors)
        , "link_east" .= f (_linkEast colors)
        , "node_highlight" .= f (_nodeHighlight colors)
        , "node_default" .= f (_nodeDefault colors)
        , "node_hover" .= f (_nodeHover colors)
        , "node_active" .= f (_nodeActive colors)
        , "node_pilgrim_default" .= f (_nodePilgrimDefault colors)
        , "node_pilgrim_hover" .= f (_nodePilgrimHover colors)
        , "node_pilgrim_active" .= f (_nodePilgrimActive colors)
        , "node_path_default" .= f (_nodePathDefault colors)
        , "node_path_hover" .= f (_nodePathHover colors)
        , "node_path_active" .= f (_nodePathActive colors)
        , "node_goal_default" .= f (_nodeGoalDefault colors)
        , "node_goal_hover" .= f (_nodeGoalHover colors)
        , "node_goal_active" .= f (_nodeGoalActive colors)
        ] where
            f color = RGB r g b where
                r = (color :: Color) ^. L.r
                g = (color :: Color) ^. L.g
                b = (color :: Color) ^. L.b