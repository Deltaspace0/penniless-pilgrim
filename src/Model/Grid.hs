{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Model.Grid
    ( Grid
    , makeGrid
    , getBounds
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
    , gridMap
    ) where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Ix
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

import Model.Direction

type Point = (Int, Int)

data Grid a b = Grid
    { _gridStructure :: Seq (Seq ([a], Maybe b, Maybe b))
    , _gridBounds :: (Int, Int)
    } deriving (Eq, Show)

instance (FromJSON a, FromJSON b) => FromJSON (Grid a b) where
    parseJSON v = do
        structure' <- parseJSON v
        let w = Seq.length $ Seq.index structure' 0
            h = Seq.length structure'
        return $ Grid
            { _gridStructure = structure'
            , _gridBounds = (w-1, h-1)
            }

instance (ToJSON a, ToJSON b) => ToJSON (Grid a b) where
    toJSON grid = toJSON $ _gridStructure grid

makeLensesWith abbreviatedFields 'Grid

point :: Point -> Lens' (Seq (Seq a)) a
point (x, y) = lens getter setter where
    getter s = Seq.index (Seq.index s y) x
    setter s v = Seq.adjust (Seq.update x v) y s

chunkNode :: Point -> Lens' (Grid a b) [a]
chunkNode p = structure . point p . _1

chunkLink :: Point -> Direction -> Lens' (Grid a b) (Maybe b)
chunkLink p d = if d `elem` [West, East]
    then structure . point (min p $ getPositionInDirection d p) . _2
    else structure . point (min p $ getPositionInDirection d p) . _3

makeGrid :: Int -> Int -> Grid a b
makeGrid w h = grid where
    grid = Grid
        { _gridStructure = Seq.replicate h $ Seq.replicate w chunk
        , _gridBounds = (w-1, h-1)
        }
    chunk = ([], Nothing, Nothing)

getBounds :: Grid a b -> (Int, Int)
getBounds = _gridBounds

isInside :: Point -> Grid a b -> Bool
isInside (x, y) grid = checkX && checkY where
    checkX = x >= 0 && x <= cols
    checkY = y >= 0 && y <= rows
    (cols, rows) = _gridBounds grid

getNode :: Point -> Grid a b -> [a]
getNode p grid = grid ^. chunkNode p

setNode :: Point -> [a] -> Grid a b -> Grid a b
setNode p node = chunkNode p .~ node

nodePeek :: Point -> Grid a b -> a
nodePeek p grid = head $ grid ^. chunkNode p

nodePush :: (Point, a) -> Grid a b -> Grid a b
nodePush (p, t) = chunkNode p %~ (t:)

nodePop :: Point -> Grid a b -> Grid a b
nodePop p = chunkNode p %~ tail

nodeTransfer :: Point -> Point -> Grid a b -> Grid a b
nodeTransfer p p' grid = let t = nodePeek p grid in
    nodePush (p', t) $ nodePop p grid

getLink :: Point -> Direction -> Grid a b -> Maybe b
getLink p d grid = grid ^. chunkLink p d

setLink :: Point -> Direction -> Maybe b -> Grid a b -> Grid a b
setLink p d link = chunkLink p d .~ link

getHlink :: Point -> Grid a b -> Maybe b
getHlink p grid = grid ^. chunkLink p East

setHlink :: Point -> Maybe b -> Grid a b -> Grid a b
setHlink p link = chunkLink p East .~ link

getVlink :: Point -> Grid a b -> Maybe b
getVlink p grid = grid ^. chunkLink p South

setVlink :: Point -> Maybe b -> Grid a b -> Grid a b
setVlink p link = chunkLink p South .~ link

getNodeIndices :: Grid a b -> [Point]
getNodeIndices grid = range ((0, 0), grid ^. bounds)

getHlinkIndices :: Grid a b -> [Point]
getHlinkIndices grid = range ((0, 0), grid ^. bounds & _1 -~ 1)

getVlinkIndices :: Grid a b -> [Point]
getVlinkIndices grid = range ((0, 0), grid ^. bounds & _2 -~ 1)

getNodeSequence :: Grid a b -> Seq (Point, [a])
getNodeSequence grid = Seq.fromList $ f <$> xs where
    xs = getNodeIndices grid
    f p = (p, grid ^. chunkNode p)

getHlinkSequence :: Grid a b -> Seq (Point, Maybe b)
getHlinkSequence grid = Seq.fromList $ f <$> xs where
    xs = getHlinkIndices grid
    f p = (p, grid ^. chunkLink p East)

getVlinkSequence :: Grid a b -> Seq (Point, Maybe b)
getVlinkSequence grid = Seq.fromList $ f <$> xs where
    xs = getVlinkIndices grid
    f p = (p, grid ^. chunkLink p South)

gridMap
    :: ([a] -> [a'])
    -> (Maybe b -> Maybe b')
    -> (Maybe b -> Maybe b')
    -> Grid a b
    -> Grid a' b'
gridMap fn fh fv grid = newGrid where
    newGrid = grid
        { _gridStructure = newStructure
        }
    newStructure = _gridStructure grid & traverse . traverse %~ f
    f (node, hlink, vlink) = (fn node, fh hlink, fv vlink)
