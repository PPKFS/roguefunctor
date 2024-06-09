module Rogue.Events
  ( handleEvents
  , BlockingMode(..)
  ) where

import Rogue.Prelude

data BlockingMode = Blocking | NotBlocking
  deriving stock (Eq, Ord, Generic, Show, Bounded, Enum)

handleEvents :: MonadIO m => BlockingMode -> (Event -> m a) -> m [a]
handleEvents bm f = do
  let allEvents :: MonadIO m => m [Event]
      allEvents = go True
        where
          go isEmptySoFar = do
            i <- terminalHasInput
            if i || (bm == Blocking && isEmptySoFar)
            then do
              r <- terminalRead
              (r :) <$> go False
            else
              pure []
  ev <- allEvents
  mapM f ev