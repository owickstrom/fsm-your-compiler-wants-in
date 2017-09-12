module Main

import Game
import Checkout
import Control.ST
import Effects
import Effect.System

main : IO ()
main =
  case !(run getArgs) of
    [_, "game"] => runGame
    [_, "checkout"] => runCheckout
    _ => putStrLn "Usage: examples (game|checkout)"
