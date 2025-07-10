module Rogue.AStar where

import Data.Graph.AStar
import Rogue.Prelude
import Rogue.Tilemap
import qualified Data.HashSet as HS

findPath ::
  WalkabilityMap map
  => map
  -> V2
  -> V2
  -> Maybe [V2]
findPath tilemap start goal = do
  aStar
    (findValidExits tilemap start goal)
    (const $ const 1)
    (squaredDistance goal)
    (==goal)
    start

getCompassDirections :: V2 -> [V2]
getCompassDirections (V2 x y) = [V2 x' y' | x' <- [(x-1)..(x+1)], y' <- [(y-1)..(y+1)], (x', y') /= (x, y)]

findValidExits :: WalkabilityMap map => map -> V2 -> V2 -> V2 -> HashSet V2
findValidExits m start goal x = filter (\p -> start == p || goal == p || (not . positionBlocksMovement m $ p) ) (getCompassDirections x) & HS.fromList
