module Game

import Control.ST
import Sleep

namespace Protocol
  -- An abstract state type for the protocol.
  data HeroState = Standing | Jumping | Ducking

  -- The hero can only jump when already standing or ducking.
  jumpTransition : HeroState -> HeroState
  jumpTransition Standing = Jumping
  jumpTransition Ducking = Jumping
  jumpTransition s = s

  -- The Hero state machine, with actions and the legal state
  -- transitions.
  interface Hero (m : Type -> Type) where
    State : HeroState -> Type

    spawn : ST m Var [add (State Standing)]

    jump : (hero : Var) -> ST m () [hero ::: State s
                                         :-> State (jumpTransition s)]

    land : (hero : Var) -> ST m () [hero ::: State Jumping
                                         :-> State Standing]

    duck : (hero : Var) -> ST m () [hero ::: State Standing
                                         :-> State Ducking]

    rise : (hero : Var) -> ST m () [hero ::: State Ducking
                                         :-> State Standing]

    perish : (hero : Var) -> ST m () [remove hero (State Standing)]


namespace Implementation

  -- A concrete state type for the implementation.
  data HeroState : Protocol.HeroState -> Type where
    Standing : HeroState Protocol.Standing
    Jumping : (meters : Nat) -> HeroState Protocol.Jumping
    Ducking : HeroState Protocol.Ducking

  -- An implementation for hero that "renders" by printing out
  -- information to the console.
  implementation Hero IO where
    State = Control.ST.State . Implementation.HeroState

    spawn = do
      putStrLn "Hero spawned, standing strong."
      new Standing

    jump hero = do
      st <- read hero
      case st of
        Ducking => do
          putStrLn "Mini-jump!"
          write hero (Jumping 1)
        Standing => do
          putStrLn "Weeeee, we're in the air!"
          write hero (Jumping 3)

    land hero = do
      Jumping height <- read hero
      lift (sleepSeconds height)
      putStrLn "Safely back on ground."
      write hero Standing

    duck hero = do
      putStrLn "Crouching hero, hidden implementation."
      write hero Ducking

    rise hero = do
      putStrLn "The hero has risen."
      write hero Standing

    perish hero = do
      putStrLn "Hero died."
      delete hero

-- The "driver" program, reading commands from the console and causing
-- state transitions. This program cannot do illegal state transitions
-- (would cause type errors).
using (ConsoleIO m, Hero m)
  mutual
    total
    inStanding
      : (hero : Var)
      -> STLoop m () [remove hero (State {m} Standing)]
    inStanding hero = do
      case !getStr of
        "jump" => do
          jump hero
          inJumping hero
        "duck" => do
          duck hero
          inDucking hero
        "die" => do
          perish hero
          pure ()
        cmd => do
          putStrLn ("Invalid command: " ++ cmd)
          inStanding hero

    total
    inDucking
      : (hero : Var)
      -> STLoop m () [remove hero (State {m} Ducking)]
    inDucking hero = do
      case !getStr of
        "jump" => do
          jump hero
          inJumping hero
        "rise" => do
          rise hero
          inStanding hero
        cmd => do
          putStrLn ("Invalid command: " ++ cmd)
          inDucking hero

    total
    inJumping
      : (hero : Var)
      -> STLoop m () [remove hero (State {m} Jumping)]
    inJumping hero = do
      land hero
      inStanding hero


total
prg : (ConsoleIO m, Hero m) => STransLoop m () [] (const [])
prg = do
  hero <- spawn
  inStanding hero


-- Run the hero program using IO.
export
runGame : IO ()
runGame =
  runLoop forever prg (putStrLn "Thanks for playing!")
