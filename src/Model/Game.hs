{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Game
    ( module Model.Game.GameLink
    , module Model.Game.GameNode
    , module Model.Game.Pilgrim
    , Game(..)
    , makeGame
    , makeGame_
    , movePilgrim
    , jumpPilgrim
    , applyPath
    , transferPath
    , directionFromGame
    , taxFromGame
    ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Default
import Data.Maybe
import Monomer.SaveManager

import Common
import Model.Game.GameLink
import Model.Game.GameNode
import Model.Game.Pilgrim
import Composites.GameControl.ControlledGame

data Game = Game
    { _grid :: Grid GameNode GameLink
    , _pilgrim :: Pilgrim
    } deriving (Eq, Show)

instance FromJSON Game where
    parseJSON = withObject "Game" $ \v -> Game
        <$> v .: "grid"
        <*> v .: "pilgrim"

instance ToJSON Game where
    toJSON Game{..} = object
        [ "grid" .= _grid
        , "pilgrim" .= _pilgrim
        ]

instance ControlledGame Game where
    getCurrentPosition = _position . _pilgrim
    getScoreByPosition = taxFromGame
    getPreviousPositions = fmap fst . _path . _pilgrim
    moveToDirection = movePilgrim
    moveToPosition = jumpPilgrim

instance FromFile (Saves Game)
instance ToFile (Saves Game)

makeGame :: Int -> Int -> Game
makeGame w h = makeGame_ w h def

makeGame_ :: Int -> Int -> Pilgrim -> Game
makeGame_ w h pilgrim = Game
    { _grid = foldr nodePush (makeGrid w h)
        [ (_position pilgrim, NodePilgrim)
        , ((w-1, h-1), NodeGoal)
        ]
    , _pilgrim = pilgrim
    }

movePilgrim :: Direction -> Game -> Maybe Game
movePilgrim d Game{..} = result where
    result = if inside && (goingBack || linkEmpty)
        then Just $ Game
            { _grid = nodeUpdate $ setLink p d link _grid
            , _pilgrim = updatePilgrim d _pilgrim
            }
        else Nothing
    p = _position _pilgrim
    p' = getPositionInDirection d p
    inside = isInside p' _grid
    goingBack = isOppositeToPilgrim d _pilgrim
    linkEmpty = null $ getLink p d _grid
    nodeUpdate = if goingBack
        then nodeTransfer p p' . nodePop p'
        else nodePush (p, NodePath tax) . nodeTransfer p p'
    tax = _tax _pilgrim
    link | goingBack = Nothing
         | p == min p p' = Just LinkForward
         | otherwise = Just LinkBack

jumpPilgrim :: (Int, Int) -> Game -> Maybe Game
jumpPilgrim p game
    | not (null direction) = movePilgrim (fromJust direction) game
    | elem p (map fst path) = applyPath directions game
    | otherwise = Nothing
    where
        direction = directionFromGame p game
        path = _path $ _pilgrim game
        directions = reverse $ map f $ ds ++ [head ds']
        f = getOppositeDirection . snd
        (ds, ds') = span ((/= p) . fst) path

applyPath :: [Direction] -> Game -> Maybe Game
applyPath directions game = foldr f (Just game) directions where
    f = (=<<) . movePilgrim

transferPath :: Game -> Game -> Maybe Game
transferPath oldGame game = applyPath directions game where
    directions = map snd $ _path $ _pilgrim oldGame

directionFromGame :: (Int, Int) -> Game -> Maybe Direction
directionFromGame (x, y) game
    | null direction = Nothing
    | null (movePilgrim (fromJust direction) game) = Nothing
    | otherwise = direction
    where
        direction = getRelativeDirection p (x, y)
        p = _position $ _pilgrim game

taxFromGame :: (Int, Int) -> Game -> Maybe Double
taxFromGame p game = moveResult <|> nodeResult where
    moveResult = _tax . _pilgrim <$> nextGame
    nextGame = directionFromGame p game >>= (flip movePilgrim) game
    nodeResult = getTax $ getNode p $ _grid game
    getTax [] = Nothing
    getTax (NodePath tax:_) = Just tax
    getTax (_:xs) = getTax xs
