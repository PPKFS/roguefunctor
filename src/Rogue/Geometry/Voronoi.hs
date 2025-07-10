module Rogue.Geometry.Voronoi where

import Fortune
import qualified Data.Set as S
import qualified Data.IntMap as IM
import qualified Data.List.NonEmpty as NE
import Rogue.Prelude
import Rogue.Geometry.Rectangle

data Region = Region
  { regionId :: Int
  , edges :: [(V2, V2, Int)]
  }
data Voronoi = Voronoi
  { regions :: IM.IntMap Region
  , slabs :: IM.IntMap [Region]
  , slabWidth :: Rational
  }

v2ToPoint :: V2 -> (Double, Double)
v2ToPoint (V2 x y) = (fromIntegral x, fromIntegral y)

pointToV2 :: (Double, Double) -> V2
pointToV2 (x, y) = V2 (round x) (round y)

makeVoronoi :: Rectangle -> [V2] -> Int -> Voronoi
makeVoronoi bbox pts slabs =
  let es = voronoi (v2ToPoint $ topLeft bbox, v2ToPoint $ topRight bbox) (map v2ToPoint pts)
      is = zipWith const [0..] pts
      ss = [0 .. slabs - 1]
      slabWidth = fromIntegral (width bbox) / fromIntegral slabs
      regionList = map (\i -> Region { regionId = i, edges = map (\ (Edge' r1 r2 p1 p2) -> (pointToV2 p1, pointToV2 p2, if r1 == i then r2 else r1)) (filter (\(Edge' r1 r2 _ _) -> r1 == i || r2 == i) es) }) is
  in Voronoi
      { regions = IM.fromList (zip is regionList)
      , slabs = IM.fromList (map (\sid ->
          let (lSlab :: Rational) = fromIntegral sid * slabWidth
              (rSlab :: Rational) = fromIntegral (sid+1) * slabWidth
          in (sid, filter (\Region{edges} -> any (\(p1, p2, _) -> let betweenThem (V2 p _) = fromIntegral p >= lSlab && fromIntegral p < rSlab in betweenThem p1 || betweenThem p2) edges) regionList)) ss)
      , slabWidth = slabWidth
      }