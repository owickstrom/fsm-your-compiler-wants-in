{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Concurrent.Async.Lifted
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Prelude                         hiding (log)
import qualified System.Timeout.Lifted           as Timeout

data Open
data Closed

class Door m where
  type State m :: * -> *
  initial :: m (State m Closed)
  open :: State m Closed -> m (State m Open)
  close :: State m Open -> m (State m Closed)
  timeout :: State m Open -> m (State m Closed)

data Command
  = OpenDoor
  | CloseDoor
  deriving (Show, Read, Eq)

log :: MonadBaseControl IO m => String -> m ()
log = liftBase . putStrLn

nextCommand :: MonadBaseControl IO m => m Command
nextCommand = do
  line <- liftBase getLine
  case line of
    "open" -> return OpenDoor
    "close" -> return CloseDoor
    s         -> do
      log ("Invalid event: " ++ s)
      nextCommand

seconds :: Int -> Int
seconds = (*) 1000000

onClosed ::
     (MonadBaseControl IO m, Monad m, Door m)
  => State m Closed
  -> m (State m Closed)
onClosed door = do
  log "Door is closed."
  e <- nextCommand
  case e of
    OpenDoor  -> open door >>= onOpen
    CloseDoor -> onClosed door

onOpen ::
     (MonadBaseControl IO m, Monad m, Door m)
  => State m Open
  -> m (State m Closed)
onOpen door = do
  log "Door is open."
  Timeout.timeout (seconds 5) nextCommand >>= \case
    Just ev ->
      case ev of
        CloseDoor -> close door >>= onClosed
        OpenDoor  -> onOpen door
    Nothing -> do
      log "Closed automatically."
      close door >>= onClosed

prg ::
     (MonadBaseControl IO m, Monad m, Door m)
  => m ()
prg = do
  door <- initial
  _ <- onClosed door
  log "Bye!"
  return ()


newtype DoorImpl m a = DoorImpl
  { runDoorImpl :: m a
  } deriving (Monad, Applicative, Functor)

instance MonadBase IO m => MonadBase IO (DoorImpl m) where
  liftBase = DoorImpl . liftBase

instance MonadBaseControl IO m => MonadBaseControl IO (DoorImpl m) where
  type StM (DoorImpl m) a = StM m a
  liftBaseWith f = DoorImpl $ liftBaseWith $ \r -> f (r . runDoorImpl)
  restoreM = DoorImpl . restoreM

data StateImpl s where
  Open :: StateImpl Open
  Closed :: StateImpl Closed

instance Monad m => Door (DoorImpl m) where
  type State (DoorImpl m) = StateImpl
  initial = return Closed
  open _ = return Open
  close _ = return Closed
  timeout _ = return Closed

main :: IO ()
main = runConcurrently (Concurrently (runDoorImpl prg))
