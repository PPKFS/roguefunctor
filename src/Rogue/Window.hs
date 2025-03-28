module Rogue.Window
  ( initWindow
  , withWindow

  ) where
import Rogue.Prelude
import Rogue.Config
import Control.Monad.Catch

initWindow :: MonadIO m => WindowOptions -> m ()
initWindow opts = do
  void $ terminalOpen
  void $ terminalSetOptions opts

withWindow :: HasCallStack => MonadMask m => MonadIO m => WindowOptions -> m a -> (a -> m b) -> m c -> m b
withWindow opts initialise loop exit = bracket
  (initWindow opts >> initialise)
  (const $ exit >> terminalClose)
  loop
