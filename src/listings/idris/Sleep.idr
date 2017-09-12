module Sleep

import System

export
sleepSeconds : Nat -> IO ()
sleepSeconds Z = pure ()
sleepSeconds (S n) = do
  usleep 1000000
  sleepSeconds n
