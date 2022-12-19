{-# LANGUAGE OverloadedStrings #-}

module Model.Game
    ( module Model.Game.Link
    , module Model.Game.Node
    , module Model.Grid
    , module Model.Pilgrim
    , Game(..)
    , makeGame
    , makeGame_
    , movePilgrim
    , jumpPilgrim
    , applyPath
    , directionFromGame
    , taxFromGame
    ) where

import Data.Aeson
import Data.Default
import Data.Maybe

import Model.Game.Link
import Model.Game.Node
import Model.Grid
import Model.Pilgrim

data Game = Game
    { _grid :: Grid Node Link
    , _pilgrim :: Pilgrim
    } deriving (Eq, Show)

instance FromJSON Game where
    parseJSON = withObject "Game" $ \v -> Game
        <$> v .: "grid"
        <*> v .: "pilgrim"

instance ToJSON Game where
    toJSON game = object
        [ "grid" .= _grid game
        , "pilgrim" .= _pilgrim game
        ]

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
movePilgrim d game = result where
    result = if inside && (goingBack || linkEmpty)
        then Just $ Game
            { _grid = nodeUpdate $ setLink p d link grid
            , _pilgrim = updatePilgrim d pilgrim
            }
        else Nothing
    grid = _grid game
    pilgrim = _pilgrim game
    p = _position pilgrim
    p' = nextPosition d p
    inside = isInside p' grid
    goingBack = isBack d pilgrim
    linkEmpty = null $ getLink p d grid
    nodeUpdate = if goingBack
        then nodeTransfer p p' . nodePop p'
        else nodePush (p, NodePath) . nodeTransfer p p'
    link | goingBack = Nothing
         | p == min p p' = Just LinkForward
         | otherwise = Just LinkBack

jumpPilgrim :: (Int, Int) -> Game -> Maybe Game
jumpPilgrim p game
    | not (null direction) = movePilgrim (fromJust direction) game
    | elem p previousPositions = applyPath (reverse directions) game
    | otherwise = Nothing
    where
        direction = directionFromGame p game
        previousPositions = _previousPositions $ _pilgrim game
        path = _path $ _pilgrim game
        f d = fromJust . (movePilgrim d)
        directions = map (getOpposite . snd) $ ds ++ [head ds']
        (ds, ds') = span ((/= p) . fst) $ zip previousPositions path

applyPath :: [Direction] -> Game -> Maybe Game
applyPath directions game = foldr f (Just game) directions where
    f d game' = if null game'
        then Nothing
        else movePilgrim d $ fromJust game'

directionFromGame :: (Int, Int) -> Game -> Maybe Direction
directionFromGame (x, y) game
    | null direction = Nothing
    | null (movePilgrim (fromJust direction) game) = Nothing
    | otherwise = direction
    where
        direction = relativeDirection p (x, y)
        p = _position $ _pilgrim game

taxFromGame :: (Int, Int) -> Game -> Maybe Double
taxFromGame p game = tax where
    tax = if null game'
        then Nothing
        else Just $ _tax $ _pilgrim $ fromJust game'
    game' = jumpPilgrim p game