module Rogue.Colour
  ( Colour(..)
  , fromARGB
  , fromRGB
  , terminalBkColor
  , terminalBkColour
  , terminalColor
  , terminalColour

  ) where

import Rogue.Prelude
import Data.Bits
import Data.Ix
import Data.Ord (clamp)
import Foreign.C (CUInt(..))

newtype Colour = Colour { toWord32 :: Word32 }
  deriving stock (Generic)
  deriving newtype (Show, Read, Eq, Ord, Bits, FiniteBits, Num, Enum, Bounded, Ix, Real, Integral)

type Color = Colour

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
terminalColour = terminalColorUInt . CUInt . toWord32

terminalColor :: MonadIO m => Color -> m ()
terminalColor = terminalColour

terminalBkColour :: MonadIO m => Colour -> m ()
terminalBkColour = terminalBkColorUInt . CUInt . toWord32

terminalBkColor :: MonadIO m => Color -> m ()
terminalBkColor = terminalColour