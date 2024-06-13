module Rogue.FieldOfView.Raycasting where

import Rogue.Prelude
import Rogue.Geometry.V2
import qualified Data.Set as S
import Rogue.Geometry.Line
import Rogue.FieldOfView.Visibility

calculateFov :: VisibilityMap tilemap => tilemap -> V2 -> Int -> S.Set V2
calculateFov tm o = S.fromList . mconcat . map (takeWhile (not . positionBlocksVisibility tm)) . circleRays o