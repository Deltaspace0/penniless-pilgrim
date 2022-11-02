module Model.Game
    ( module Model.Pilgrim
    , Node(..)
    , Link(..)
    , Game(..)
    , makeGame
    , makeGame_
    , movePilgrim
    , movePilgrim_
    , directionFromGame
    , taxFromGame
    ) where

import Data.Default
import Data.Maybe

import Model.Grid
import Model.Pilgrim

data Node
    = NodePilgrim
    | NodePath
    | NodeGoal
    deriving (Eq, Show)

data Link = LinkBack | LinkForward deriving (Eq, Show)

data Game = Game
    { _grid :: Grid Node Link
    , _pilgrim :: Pilgrim
    } deriving (Eq, Show)

makeGame :: Int -> Int -> Game
makeGame w h = makeGame_ w h def

makeGame_ :: Int -> Int -> Pilgrim -> Game
makeGame_ w h pilgrim = Game
    { _grid = nodePush (_position pilgrim, NodePilgrim) grid
    , _pilgrim = pilgrim
    } where
        grid = makeGrid w h

movePilgrim :: Direction -> Game -> Game
movePilgrim d game = fst $ movePilgrim_ d game

movePilgrim_ :: Direction -> Game -> (Game, Bool)
movePilgrim_ d game = if inside && (goingBack || linkEmpty)
    then (game', True)
    else (game, False) where
        game' = Game
            { _grid = nodeUpdate $ setLink p d link grid
            , _pilgrim = updatePilgrim d pilgrim
            }
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

directionFromGame :: (Int, Int) -> Game -> Maybe Direction
directionFromGame (x, y) game
    | null direction = Nothing
    | snd (movePilgrim_ (fromJust direction) game) = direction
    | otherwise = Nothing
    where
        direction = relativeDirection p (x, y)
        p = _position $ _pilgrim game

taxFromGame :: (Int, Int) -> Game -> Maybe Double
taxFromGame p game
    | null direction = Nothing
    | otherwise = Just $ _tax $ _pilgrim game'
    where
        direction = directionFromGame p game
        game' = movePilgrim (fromJust direction) game