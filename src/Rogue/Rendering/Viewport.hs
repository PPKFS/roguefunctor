module Rogue.Rendering.Viewport where
import Rogue.Geometry.Rectangle
import Rogue.Prelude hiding (Reader, runReader, ask)
import Effectful.Reader.Static
import Rogue.Colour
import Rogue.Geometry.V2
import qualified Data.Set as S

type Layer = Word8

bgLayer :: Layer
bgLayer = 0

-- | a viewport is some rectangular area over some amount of layers
-- two viewports cannot clash in both layer and area, so each one can be cleared and rendered
-- independently

data Viewport layer = Viewport
  { viewport :: Rectangle
  , dirtyLayers :: S.Set layer
  , backgroundLayer :: Maybe (layer, Colour)
  }

class AsLayer layer where
  toLayer :: layer -> Word8

terminalLayer' :: IOE :> es => Word8 -> Eff es ()
terminalLayer' = terminalLayer . fromIntegral

withViewport ::
  Viewport l
  -> Eff (Reader (Viewport l) : es) a
  -> Eff es a
withViewport = runReader

clearViewport ::
  IOE :> es
  => AsLayer l
  => Eq l
  => Viewport l
  -> Eff es ()
clearViewport v = forM_ (dirtyLayers v) $ \dl -> do
  terminalLayer' (toLayer dl)
  -- if this is the global background layer
  when (toLayer dl == bgLayer) $
    case backgroundLayer v of
      Nothing -> error "viewport had a background layer but no colour"
      Just (_, c) -> terminalBkColour c
  withV2 (rectangleDimensions (viewport v)) $ withV2 (topLeft $ viewport v) terminalClearArea
  when ((Just dl == (fst <$> backgroundLayer v)) && toLayer dl /= bgLayer) $ do
    -- TODO: this is where we would want to replace the background layer with just coloured squares
    pass

withViewportTransform ::
  Reader (Viewport l) :> es
  => V2
  -> (Int -> Int -> Eff es a)
  -> Eff es a
withViewportTransform p f = do
  r <- ask
  withV2 (topLeft (viewport r) + p) f

viewportDrawTile ::
  IOE :> es
  => AsLayer l
  => Reader (Viewport l) :> es
  => V2
  -> Maybe l
  -> Colour
  -> Char
  -> Eff es ()
viewportDrawTile p mbL fg glyph = do
  whenJust mbL $ do
    terminalLayer' . toLayer
  terminalColour fg
  void $ withViewportTransform p (\x y -> terminalPrintText x y (one glyph))
