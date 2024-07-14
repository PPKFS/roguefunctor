{-# LANGUAGE RecordWildCards #-}
module Rogue.Rendering.Viewport where
import Rogue.Geometry.Rectangle
import Rogue.Prelude hiding (Reader, runReader, ask)
import Effectful.Reader.Static
import Rogue.Colour
import Data.Ix (Ix(inRange))

type Layer = Word8

bgLayer :: Layer
bgLayer = 0

-- | a viewport is some rectangular area over some amount of layers
-- two viewports cannot clash in both layer and area, so each one can be cleared and rendered
-- independently

data Viewport layer = Viewport
  { viewport :: Rectangle
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
  forall l es.
  IOE :> es
  => AsLayer l
  => Eq l
  => Enum l
  => Bounded l
  => Viewport l
  -> Eff es ()
clearViewport v = forM_ (universe @l) $ \dl -> do
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

whenInViewport ::
  MonadIO m
  => Viewport l
  -> V2
  -> m ()
  -> m ()
whenInViewport v p = when (inRange (V2 0 0, V2 1 1 + rectangleDimensions (viewport v)) p)

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

data BorderTileSet = BTS
  { tl :: Char
  , tr :: Char
  , bl :: Char
  , br :: Char
  , l :: Char
  , r :: Char
  , t :: Char
  , b :: Char
  }

unicodeBorders :: BorderTileSet
unicodeBorders = BTS
  { tl = '╔'
  , tr = '╗'
  , bl = '╚'
  , br = '╝'
  , l = '║'
  , r = '║'
  , t = '═'
  , b = '═'
  }

borderViewport ::
  IOE :> es
  => Reader (Viewport l) :> es
  => Colour
  -> BorderTileSet
  -> Eff es ()
borderViewport c BTS{..} = do
  (Viewport rect _) <- ask
  let f x y glyph = terminalPrintText x y (one glyph)
  terminalColour c
  withV2 (topLeft rect) f tl
  withV2 (bottomRight rect - V2 1 1) f br
  withV2 (topRight rect - V2 1 0) f tr
  withV2 (bottomLeft rect - V2 0 1) f bl
  forM_ [view _1 (topLeft rect) .. view _1 (topRight rect) - 2 ] $ \x -> do
    f x (rect ^. #topLeft % _2) t
    f x (rect ^. #bottomRight % _2 - 1) b
  forM_ [view _2 (topLeft rect) .. view _2 (bottomLeft rect) - 2 ] $ \y -> do
    f (rect ^. #topLeft % _1) y l
    f (rect ^. #bottomRight % _1 - 1) y r

data SomeViewport es where
  SomeViewport :: (IOE :> es, AsLayer l, Eq l, Enum l, Bounded l) => Viewport l -> Eff (Reader (Viewport l) : es) () -> SomeViewport es

renderViewport ::
  SomeViewport es
  -> Eff es ()
renderViewport (SomeViewport v f) = withViewport v $ do
  clearViewport v
  f