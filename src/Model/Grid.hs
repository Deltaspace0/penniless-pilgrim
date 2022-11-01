{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Model.Grid
    ( Grid(..)
    , makeGrid
    , isInside
    , getNode
    , setNode
    , nodePeek
    , nodePush
    , nodePop
    , nodeTransfer
    , getLink
    , setLink
    , getHlink
    , setHlink
    , getVlink
    , setVlink
    , getNodeIndices
    , getHlinkIndices
    , getVlinkIndices
    , getNodeSequence
    , getHlinkSequence
    , getVlinkSequence
    , getBounds
    , gridMap
    ) where

import Control.Lens hiding (indices)
import Data.Array.IArray
import Data.Maybe
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

import Model.Direction

type Point = (Int, Int)

data Grid a b = Grid
    { _gridNodes  :: Array Point [a]
    , _gridHlinks :: Array Point (Maybe b)
    , _gridVlinks :: Array Point (Maybe b)
    } deriving (Eq, Show)

makeLensesWith abbreviatedFields 'Grid

makeGrid :: Int -> Int -> Grid a b
makeGrid w h = grid where
    hbounds = ((0, 0), (w-2, h-1))
    vbounds = ((0, 0), (w-1, h-2))
    ibounds = ((0, 0), (w-1, h-1))
    grid = Grid
        { _gridNodes  = listArray ibounds $ repeat []
        , _gridHlinks = listArray hbounds $ repeat Nothing
        , _gridVlinks = listArray vbounds $ repeat Nothing
        }

isInside :: Point -> Grid a b -> Bool
isInside (x, y) grid = checkX && checkY where
    checkX = x >= 0 && x <= cols
    checkY = y >= 0 && y <= rows
    (cols, rows) = snd $ bounds $ grid ^. nodes

getNode :: Point -> Grid a b -> [a]
getNode p grid = grid ^. nodes . ix p

setNode :: Point -> [a] -> Grid a b -> Grid a b
setNode p node = nodes . ix p .~ node

nodePeek :: Point -> Grid a b -> a
nodePeek p grid = head $ grid ^. nodes . ix p

nodePush :: (Point, a) -> Grid a b -> Grid a b
nodePush (p, t) = nodes . ix p %~ (t:)

nodePop :: Point -> Grid a b -> Grid a b
nodePop p = nodes . ix p %~ tail

nodeTransfer :: Point -> Point -> Grid a b -> Grid a b
nodeTransfer p p' grid = let t = nodePeek p grid in
    nodePush (p', t) $ nodePop p grid

getLink :: Point -> Direction -> Grid a b -> Maybe b
getLink p d grid = if d `elem` [West, East]
    then (grid ^. hlinks)!p'
    else (grid ^. vlinks)!p' where
        p' = min p $ nextPosition d p

setLink :: Point -> Direction -> Maybe b -> Grid a b -> Grid a b
setLink p d link = if d `elem` [West, East]
    then hlinks . ix p' .~ link
    else vlinks . ix p' .~ link where
        p' = min p $ nextPosition d p

getHlink :: Point -> Grid a b -> Maybe b
getHlink p grid = (grid ^. hlinks)!p

setHlink :: Point -> Maybe b -> Grid a b -> Grid a b
setHlink p link = hlinks . ix p .~ link

getVlink :: Point -> Grid a b -> Maybe b
getVlink p grid = (grid ^. vlinks)!p

setVlink :: Point -> Maybe b -> Grid a b -> Grid a b
setVlink p link = vlinks . ix p .~ link

getNodeIndices :: Grid a b -> [Point]
getNodeIndices grid = indices $ grid ^. nodes

getHlinkIndices :: Grid a b -> [Point]
getHlinkIndices grid = indices $ grid ^. hlinks

getVlinkIndices :: Grid a b -> [Point]
getVlinkIndices grid = indices $ grid ^. vlinks

getNodeSequence :: Grid a b -> Seq (Point, [a])
getNodeSequence grid = Seq.fromList $ f <$> xs where
    xs = getNodeIndices grid
    f p = (p, (grid ^. nodes)!p)

getHlinkSequence :: Grid a b -> Seq (Point, Maybe b)
getHlinkSequence grid = Seq.fromList $ f <$> xs where
    xs = getHlinkIndices grid
    f p = (p, (grid ^. hlinks)!p)

getVlinkSequence :: Grid a b -> Seq (Point, Maybe b)
getVlinkSequence grid = Seq.fromList $ f <$> xs where
    xs = getVlinkIndices grid
    f p = (p, (grid ^. vlinks)!p)

getBounds :: Grid a b -> Point
getBounds grid = snd $ bounds $ grid ^. nodes

gridMap
    :: ([a] -> [a'])
    -> (Maybe b -> Maybe b')
    -> Grid a b
    -> Grid a' b'
gridMap nodeTransform linkTransform grid = Grid
    { _gridNodes  = amap nodeTransform $ grid ^. nodes
    , _gridHlinks = amap linkTransform $ grid ^. hlinks
    , _gridVlinks = amap linkTransform $ grid ^. vlinks
    }