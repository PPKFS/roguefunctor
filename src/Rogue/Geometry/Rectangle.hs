module Rogue.Geometry.Rectangle where

import Rogue.Geometry.V2
import Rogue.Prelude

data Rectangle = Rectangle
  { topLeft :: V2
  , bottomRight :: V2
  } deriving stock (Show, Eq, Ord, Generic)

rectangleDimensions ::
  Rectangle
  -> V2
rectangleDimensions (Rectangle tl br) = br - tl

rectangleFromDimensions ::
  V2
  -> V2
  -> Rectangle
rectangleFromDimensions tl dims = Rectangle tl (tl+dims)

rectanglesIntersect ::
  Rectangle
  -> Rectangle
  -> Bool
rectanglesIntersect r1 r2 = not $
    ((r1 ^. #topLeft % _1 >= r2 ^. #bottomRight % _1) || (r2 ^. #topLeft % _1 >= r1 ^. #bottomRight % _1))
    ||
    ((r1 ^. #topLeft % _2 >= r2 ^. #bottomRight % _2) || (r2 ^. #topLeft % _2 >= r1 ^. #bottomRight % _2))

centre ::
  Rectangle
  -> V2
centre (Rectangle (V2 x1 y1) (V2 x2 y2)) = V2 ((x1+x2) `div` 2) ((y1+y2) `div` 2)

bottomEdge ::
  Rectangle
  -> Int
bottomEdge = view _2 . bottomRight

data ScanDirection = Horizontal | Vertical
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

rectanglePoints ::
  ScanDirection
  -> Rectangle
  -> [V2]
rectanglePoints Horizontal r = do
  x <- [(r ^. #topLeft % _1) .. (r ^. #bottomRight % _1)]
  V2 x <$> [(r ^. #topLeft % _2) .. (r ^. #bottomRight % _2)]
rectanglePoints Vertical r = do
  y <- [(r ^. #topLeft % _2) .. (r ^. #bottomRight % _2)]
  (`V2` y) <$> [(r ^. #topLeft % _1) .. (r ^. #bottomRight % _1)]

rectangleEdges ::
  Rectangle
  -> [V2]
rectangleEdges r = do
  x <- [(r ^. #topLeft % _1) .. (r ^. #bottomRight % _1)]
  V2 x <$> [r ^. #topLeft % _2, r ^. #bottomRight % _2]

bottomLeft ::
  Rectangle
  -> V2
bottomLeft r = V2 (r ^. #topLeft % _1) (r ^. #bottomRight % _2)

topRight ::
  Rectangle
  -> V2
topRight r = V2 (r ^. #bottomRight % _1) (r ^. #topLeft % _2)

moveToOrigin ::
  Rectangle
  -> Rectangle
moveToOrigin r = Rectangle (V2 0 0) (bottomRight r - topLeft r)

width ::
  Rectangle
  -> Int
width r = (r ^. #bottomRight % _1 - r ^. #topLeft % _1)

height ::
  Rectangle
  -> Int
height r = (r ^. #bottomRight % _2 - r ^. #topLeft % _2)