module Rogue.Geometry.Line where

import Rogue.Geometry.V2
import Rogue.Prelude
import Data.Bits

difference :: Int -> Int -> (Int, Int)
difference xs xd = if xd >= xs then (xd - xs, 1) else (xs - xd, -1)

-- a symmetric version of bresenham's line algorithm
-- I don't know how it works but it does
tranThong :: V2 -> (V2 -> a) -> V2 -> [a]
tranThong start@(V2 xs ys) f (V2 xd yd) = let
  (dx, sx) = difference xs xd
  (dy, sy) = difference ys yd
  foreach d1 d2 tx = foldr (\_ (test', x', y', rs) -> let
    test2' = test' - d2
    x2' = if tx || test2' < 0 then x' + sx else x'
    y2' = if not tx || test2' < 0 then y' + sy else y'
    in (if test2' < 0 then test2' + d1 else test2', x2', y2', f (V2 x2' y2'):rs))
    ((d1 + if sy == 1 then (-1) else 0) `shiftR` 1, xs, ys, []) [0..(d1-1)]
  in f start:reverse (view _4 $ if dx >= dy then foreach dx dy True else foreach dy dx False)

circleRays :: V2 -> Int -> [[V2]]
circleRays o@(V2 ox oy) radius =
  let squarePoints = mconcat
        [ map (\x -> V2 x (oy - radius)) [ox - radius .. ox + radius]
        , map (\x -> V2 x (oy + radius)) [ox - radius .. ox + radius]
        , map (V2 (ox + radius)) [oy - radius .. oy + radius]
        , map (V2 (ox - radius)) [oy - radius .. oy + radius]
        ]
      sq a = a^(2 :: Int)
      validDist p = (\(V2 x y) -> sq x + sq y) (p-o) < sq radius
  in map (takeWhile validDist . tranThong o id) squarePoints
