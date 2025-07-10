module Rogue.FieldOfView.SymmetricShadowcasting where
{- INCOMPLETE
import Rogue.Prelude
import Rogue.Geometry.V2
import qualified Data.Vector as V


data Quadrant = North | South | East | West
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

transformQuadrant :: Quadrant -> V2 -> V2
transformQuadrant = error ""

data Row = Row
  { depth :: Rational
  , start :: Rational
  , end :: Rational
  }

roundUp :: Rational -> Int
roundUp r = floor (r + 0.5)

roundDown :: Rational -> Int
roundDown r = ceiling (r - 0.5)

rowTiles :: Row -> [V2]
rowTiles r =
  let minCol = roundUp (depth r * start r)
      maxCol = roundDown (depth r * end r)
  in map (V2 (fromIntegral $ numerator $ depth r)) [minCol .. maxCol]

nextRow :: Row -> Row
nextRow r = r { depth = depth r + 1 }

compute :: tilemap -> V2 -> Int -> [V2]
compute tm origin range = flip map (universe @Quadrant) $ \q ->
  let firstRow = Row 1 (-1) 1
  in scan firstRow
  where
    scan row = foldl' scanRow ([], row, Nothing) (rowTiles row)
    scanRow :: ([V2], Row, Maybe V2) -> V2 -> ([V2], Row, Maybe V2)
    scanRow (tiles, thisRow, previousTile) pos =
      let isWall t = (positionBlocksVisibility tm (quadrantTransform t))
          toAmend = if isWall pos || isSymmetric thisRow pos
            then Just (quadrantTransform pos)
            else Nothing
          newRow = if (isWall <$> previousTile) == Just True && not (isWall pos) then thisRow { start = slope pos } else thisRow
          doNextRow = if not ((isWall <$> previousTile) == Just True) && (isWall pos) then scan
      in error ""
-}

-- makeSpecifics ''Viewshed
{-
data OctantPosition = NNE | NE | SE | SSE | SSW | SW | NW | NNW

type ShadowLine = [V2]

-- this assumes y goes downwards
transformOctant :: Int -> Int -> OctantPosition -> V2
transformOctant row col = \case
  NNE -> V2 col row
  NE -> V2 row (-col)
  SE -> V2 row col
  SSE -> V2 col row
  SSW -> V2 (-col) row
  SW -> V2(-row) col
  NW -> V2 (-row) (-col)
  NNW -> V2 (-col) (-row)
-}