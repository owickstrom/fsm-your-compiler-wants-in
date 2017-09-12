module Game where

import Prelude

import Control.IxMonad (ibind)
import Control.IxMonad.Leffe (class IxMonadLeffe, Leffe, addResource, removeResource, replaceResource, runLeffe)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Symbol (class IsSymbol)
import FSM (Add, Remove, Transition, log)
import Type.Data.Symbol (SProxy(..))

data Standing = Standing

data Jumping = Jumping

spawn
  :: forall m hero
   . SProxy hero
  -> Add m hero Standing Unit
spawn hero = addResource hero Standing

jump
  :: forall m hero
   . SProxy hero
  -> Transition m hero Standing Jumping Unit
jump hero = replaceResource hero Jumping

land
  :: forall m hero
   . SProxy hero
  -> Transition m hero Jumping Standing Unit
land hero = replaceResource hero Standing

attack
  :: forall m e attacker defender attackerState defenderState ra rb r'
   . IxMonadLeffe m
  => MonadEff (console :: CONSOLE | e) (m (Record r') (Record r'))
  => IsSymbol attacker
  => IsSymbol defender
  => RowCons attacker attackerState ra r'
  => RowCons defender defenderState rb r'
  => SProxy attacker
  -> SProxy defender
  -> m (Record r') (Record r') Unit
attack attacker defender =
  log "Ouch!"

perish
  :: forall m hero s
   . SProxy hero
  -> Remove m hero s Unit
perish hero = removeResource hero

myGame
  :: forall m e
   . MonadEff (console :: CONSOLE | e) m
  => Leffe m {} {} Unit
myGame = do
  let hero = SProxy :: SProxy "Priestess"
      enemy = SProxy :: SProxy "Demon Hunter"

  spawn hero
  spawn enemy

  jump hero
  enemy `attack` hero
  land hero

  hero `attack` enemy

  perish hero
  perish enemy

  where
    bind = ibind
    discard = ibind

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runLeffe myGame
