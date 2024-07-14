module Rogue.AStar where

import Data.Graph.AStar
import Rogue.Prelude
import Rogue.Tilemap
import qualified Data.HashSet as HS

findPath ::
  Monad m
  => WalkabilityMap map
  => map
  -> V2
  -> V2
  -> m (Maybe [V2])
findPath tilemap start goal = do
  aStarM
    (findValidExits tilemap start goal)
    (const $ const $ pure 1)
    (return . squaredDistance goal)
    (return . (==goal) )
    (return start)

getCompassDirections :: V2 -> [V2]
getCompassDirections (V2 x y) = [V2 x' y' | x' <- [(x-1)..(x+1)], y' <- [(y-1)..(y+1)], (x', y') /= (x, y)]

findValidExits :: Monad m => WalkabilityMap map => map -> V2 -> V2 -> V2 -> m (HashSet V2)
findValidExits m start goal x = filter (\p -> start == p || goal == p || (not . positionBlocksMovement m $ p) ) (getCompassDirections x) & return . HS.fromList
