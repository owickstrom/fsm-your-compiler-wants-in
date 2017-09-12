-- | Utilities for finite-state machines using 'Leffe'.
module FSM (Label, label, Add, Remove, TransitionOnRows, Transition, Remain, Empty, log) where

import Prelude

import Control.IxMonad.Leffe (class IxMonadLeffe, class IxMonadLeffeTrans)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console as Console
import Data.Symbol (class IsSymbol, SProxy(..))
import Type.Row (class RowLacks)

type Label label = SProxy label

label :: forall label. Label label
label = SProxy

-- start snippet Add
type Add m label s a =
  forall t ri ro
   . IxMonadLeffe (t m)
  => IxMonadLeffeTrans t
  => IsSymbol label
  => RowLacks label ri
  => RowCons label s ri ro
  => t m {|ri} {|ro} a
-- end snippet Add

type Remove m label s a =
  forall t ri ro
   . IxMonadLeffe (t m)
  => IxMonadLeffeTrans t
  => IsSymbol label
  => RowLacks label ro
  => RowCons label s ro ri
  => t m {|ri} {|ro} a

type TransitionOnRows m ri ro label from to a =
  forall t r
   . IxMonadLeffe (t m)
  => IxMonadLeffeTrans t
  => IsSymbol label
  => RowCons label from r ri
  => RowCons label to r ro
  => t m {|ri} {|ro} a

type Transition m label from to a =
  forall ri ro. TransitionOnRows m ri ro label from to a

type Remain m label state a =
  forall t r r'
   . IxMonadLeffe (t m)
  => IxMonadLeffeTrans t
  => IsSymbol label
  => RowCons label state r r'
  => t m {|r'} {|r'} a

type Empty m a =
  forall t r
   . IxMonadLeffe (t m)
  => IxMonadLeffeTrans t
  => t m {} {} a

log
  :: forall m e r
   . IxMonadLeffe m
  => MonadEff (console :: CONSOLE | e) (m r r)
  => String
  -> m r r Unit
log = liftEff <<< Console.log
