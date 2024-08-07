module Rogue.FieldOfView.Raycasting where

import Rogue.Prelude
import qualified Data.Set as S
import Rogue.Geometry.Line
import Rogue.Tilemap

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

-- it works but it's a little iffy when it comes to working out corners...
calculateFov :: VisibilityMap tilemap => tilemap -> V2 -> Int -> S.Set V2
calculateFov tm o = S.fromList . mconcat . map (takeWhileInclusive (not . positionBlocksVisibility tm)) . circleRays o