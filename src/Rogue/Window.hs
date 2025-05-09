module Rogue.Window
  ( initWindow
  , withWindow

  ) where
import Rogue.Prelude
import Rogue.Config
import Control.Monad.Catch
import Rogue.Monad
import System.Random.Stateful

initWindow :: MonadIO m => WindowOptions -> m ()
initWindow opts = do
  void $ terminalOpen
  void $ terminalSetOptions opts

withWindow :: HasCallStack => MonadMask m => MonadIO m => WindowOptions -> StateT RogueState m a -> (a -> StateT RogueState m b) -> StateT RogueState m c -> m b
withWindow opts initialise loop exit = bracket
  (do
    initWindow opts
    g <- initStdGen
    let gen = RogueState $ AtomicGen g
    runStateT initialise gen)
  (\(_, rs) -> evalStateT (exit >> terminalClose) rs)
  (\(a, rs) -> evalStateT (loop a) rs)
