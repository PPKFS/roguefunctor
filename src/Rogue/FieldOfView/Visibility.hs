module Rogue.FieldOfView.Visibility where
import Rogue.Geometry.V2
import Rogue.Prelude
import Rogue.Property.TH
import Rogue.Property.Has
import Rogue.ObjectQuery
import qualified Data.Set as S

data Viewshed = Viewshed
  { visibleTiles :: S.Set V2
  , range :: Int
  } deriving stock (Eq, Ord, Show, Generic)

makeSpecifics ''Viewshed

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