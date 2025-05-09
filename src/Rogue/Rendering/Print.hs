module Rogue.Rendering.Print
  ( printText
  , printTextExt
  , printText_
  , printTextExt_

  , printChar
  , printCharExt
  ) where

import Rogue.Prelude
import BearLibTerminal

printText :: MonadIO m => V2 -> Text -> m Dimensions
printText v2 = withV2 v2 terminalPrint

printTextExt :: MonadIO m => V2 -> V2 -> Maybe PrintAlignment -> Text -> m Dimensions
printTextExt pos ali = withV2 ali (withV2 pos terminalPrintExt)

printText_ :: MonadIO m => V2 -> Text -> m ()
printText_ v2 = withV2 v2 terminalPrint_

printTextExt_ :: MonadIO m => V2 -> V2 -> Maybe PrintAlignment -> Text -> m ()
printTextExt_ pos ali = withV2 ali (withV2 pos terminalPrintExt_)

printChar :: MonadIO m => V2 -> Char -> m ()
printChar v2 = withV2 v2 terminalPut

-- TODO:
printCharExt :: MonadIO m => V2 -> V2 -> Char -> Maybe (Integer, Integer, Integer, Integer) -> m ()
printCharExt v2 ali = withV2 ali (withV2 v2 terminalPutExt) . ord