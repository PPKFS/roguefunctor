module Rogue.FieldOfView.Visibility where
import Rogue.Geometry.V2
import Rogue.Prelude
import qualified Data.Set as S

data Viewshed = Viewshed
  { visibleTiles :: S.Set V2
  , range :: Int
  } deriving stock (Eq, Ord, Show, Generic)

emptyViewshed :: Int -> Viewshed
emptyViewshed = Viewshed S.empty