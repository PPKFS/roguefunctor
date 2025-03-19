module Rogue.Colour
  ( Colour(..)
  , fromARGB
  , fromRGB
  , terminalBkColor
  , terminalBkColour
  , terminalColor
  , terminalColour
  , toGreyscale
  , desaturate
  , toHex
  ) where

import Rogue.Prelude
import Data.Bits
import Data.Ix
import Data.Ord (clamp)
import Numeric (showHex)

newtype Colour = Colour { toWord32 :: Word32 }
  deriving stock (Generic)
  deriving newtype (Show, Read, Eq, Ord, Bits, FiniteBits, Num, Enum, Bounded, Ix, Real, Integral)

type Color = Colour

toHex :: Colour -> Text
toHex = fromString . flip showHex "" . toWord32

fromARGB :: Word8 -> Word8 -> Word8 -> Word8 -> Colour
fromARGB a r g b = (fromIntegral a `shiftL` 24) .|. (fromIntegral r `shiftL` 16) .|. (fromIntegral g `shiftL` 8) .|. fromIntegral b

fromRGB :: Word8 -> Word8 -> Word8 -> Colour
fromRGB = fromARGB 0xFF

fromARGBFloat :: RealFrac a => RealFrac r => RealFrac g => RealFrac b => a -> r -> g -> b -> Colour
fromARGBFloat a r g b = fromARGB (clampScale a) (clampScale r) (clampScale g) (clampScale b)
  where
    clampScale :: RealFrac x => x -> Word8
    clampScale = round . (/255) . clamp (0, 1)

terminalColour :: MonadIO m => Colour -> m ()
terminalColour = terminalColorUInt . fromIntegral . toWord32

terminalColor :: MonadIO m => Color -> m ()
terminalColor = terminalColour

terminalBkColour :: MonadIO m => Colour -> m ()
terminalBkColour = terminalBkColorUInt . fromIntegral . toWord32

terminalBkColor :: MonadIO m => Color -> m ()
terminalBkColor = terminalColour

toGreyscale :: Color -> Color
toGreyscale (Colour c) =
  let b :: Float = fromIntegral (c .&. 0xFF)
      g :: Float = fromIntegral (c `shiftR` 8 .&. 0xFF)
      r :: Float = fromIntegral (c `shiftR` 16 .&. 0xFF)
      a = (c `shiftR` 24 .&. 0xFF)
      grey = round (r*0.2126) + round (g*0.7152) + round (b * 0.0722)
  in
    fromARGB (fromIntegral a) grey grey grey

desaturate :: Color -> Color
desaturate (Colour c) =
  let b = fromIntegral (c .&. 0xFF)
      g = fromIntegral (c `shiftR` 8 .&. 0xFF)
      r = fromIntegral (c `shiftR` 16 .&. 0xFF)
      a = (c `shiftR` 24 .&. 0xFF)
  in
    fromARGB (fromIntegral a) (r `div` 2) (g `div` 2) (b `div` 2)