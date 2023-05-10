{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Parameters.NodeColorConfig
    ( module Model.Parameters.NodeColors
    , NodeColorConfig(..)
    , nccDefault
    , nccPilgrim
    , nccPath
    , nccGoal
    , nodeTransform
    ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Default
import Monomer

import Model.Game
import Model.Parameters.NodeColors
import Widgets.GameControlNode

data NodeColorConfig = NodeColorConfig
    { _nccDefault :: NodeColors
    , _nccPilgrim :: NodeColors
    , _nccPath :: NodeColors
    , _nccGoal :: NodeColors
    } deriving (Eq, Show)

makeLenses 'NodeColorConfig

instance Default NodeColorConfig where
    def = NodeColorConfig
        { _nccDefault = NodeColors
            { _nodeHighlight = rgb 234 232 233
            , _nodeDefault = rgb 120 120 117
            , _nodeHover = rgb 157 157 149
            , _nodeActive = rgb 105 104 103
            }
        , _nccPilgrim = NodeColors
            { _nodeHighlight = rgb 234 232 233
            , _nodeDefault = rgb 60 247 53
            , _nodeHover = rgb 116 248 111
            , _nodeActive = rgb 36 181 31
            }
        , _nccPath = NodeColors
            { _nodeHighlight = rgb 234 232 233
            , _nodeDefault = rgb 247 105 70
            , _nodeHover = rgb 245 132 105
            , _nodeActive = rgb 185 75 48
            }
        , _nccGoal = NodeColors
            { _nodeHighlight = rgb 234 232 233
            , _nodeDefault = rgb 221 230 58
            , _nodeHover = rgb 230 238 98
            , _nodeActive = rgb 162 169 42
            }
        }

instance FromJSON NodeColorConfig where
    parseJSON = withObject "NodeColorConfig" $ \v -> NodeColorConfig
        <$> v .: "default"
        <*> v .: "pilgrim"
        <*> v .: "path"
        <*> v .: "goal"

instance ToJSON NodeColorConfig where
    toJSON NodeColorConfig{..} = object
        [ "default" .= _nccDefault
        , "pilgrim" .= _nccPilgrim
        , "path" .= _nccPath
        , "goal" .= _nccGoal
        ]

nodeTransform :: NodeColorConfig -> [GameNode] -> [NodeVisual]
nodeTransform NodeColorConfig{..} = map getVisual where
    getVisual node = colorsToVisual $ case node of
        NodePilgrim -> _nccPilgrim
        NodePath _ -> _nccPath
        NodeGoal -> _nccGoal
