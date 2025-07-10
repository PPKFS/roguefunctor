module Rogue.Events
  ( handleEvents
  , handleEvents_
  , BlockingMode(..)
  , InputEvent(..)
  , makeEvent
  ) where

import Rogue.Prelude
import qualified Data.Set as S

data BlockingMode = Blocking | NotBlocking
  deriving stock (Eq, Ord, Generic, Show, Bounded, Enum)

handleEvents :: MonadIO m => BlockingMode -> (Keycode -> m a) -> m [a]
handleEvents bm f = do
  let allEvents :: MonadIO m => m [Keycode]
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

handleEvents_ :: MonadIO m => BlockingMode -> (Keycode -> m a) -> m ()
handleEvents_ = (void .) . handleEvents

data InputEvent =
  PressedKey Keycode
  | WithModifier InputEvent Keycode
  deriving stock (Eq, Show, Ord, Generic)

type ActiveModifierKeys = S.Set Keycode

makeEvent :: MonadIO m => ActiveModifierKeys -> Keycode -> m InputEvent
makeEvent acMod kc = do
  mods <- mapMaybeM (\s -> (\case
    1 -> Just s
    _ -> Nothing) <$> terminalState s) $ S.toList acMod
  case mods of
    [] -> return (PressedKey kc)
    x -> return $ foldl' WithModifier (PressedKey kc) x