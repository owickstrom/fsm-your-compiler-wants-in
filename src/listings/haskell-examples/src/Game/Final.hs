{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RebindableSyntax           #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
module Game.Final where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Indexed
import           Control.Monad.IO.Class
import           Data.Bool
import           Data.Function
import           Data.Functor
import           Data.Int
import           Data.Kind
import           Data.Maybe
import           Data.Proxy
import           Data.String
import           Data.Type.List         (Fst, Snd)
import           Data.Type.Map
import           Prelude                (fromInteger)
import           System.IO
import GHC.TypeLits (Symbol)

-- | A directed relationship between two state types, stating that a
-- transition is legal between 'from' and 'to'. Bidirectional arrows
-- need two instances of this type family.
type family LegalTransition from to :: Bool

-- | Arrows in state diagrams are usually long, so let's have a long
-- arrow infix operator.
type from ---> to = LegalTransition from to ~ 'True

type IxStateMachine m i o a
   = IxMonad m =>
       m (Map i) (Map o) a

class FsmMonad m where
  type State m :: * -> *
  freturn :: forall (xs :: [Mapping Symbol *]) a. a -> m (Map xs) (Map xs) a
  fbind :: forall i j k a b. m (Map i) (Map j) a -> (a -> m (Map j) (Map k) b) -> m (Map i) (Map k) b
  new ::
       Lookup xs k ~ Nothing
    => State m s
    -> m (Map xs) (Map ((k ':-> State m s) ': xs)) (Proxy k)
  -- TODO: Constrain legal transition!
  enter ::
       Proxy k
    -> State m o
    -> m (Map (AsMap ((k ':-> State m i) ': xs))) (Map (AsMap ((k ':-> State m o) ': xs))) ()
  delete ::
       xs :\ k ~ xs' => Proxy k -> m (Map xs) (Map xs') ()

fdiscard a b = (fbind a (const b))

type Add m (l :: Symbol) s a
   = forall xs. (FsmMonad m, Lookup xs l ~ Nothing) =>
                  State m s -> m (Map xs) (Map ((l ':-> State m s) ': xs)) a

type Remain m (l :: Symbol) s a
   = forall xs. (FsmMonad m) =>
                  m (Map ((l ':-> State m s) ': xs)) (Map ((l ':-> State m s) ': xs)) a

type Transition m (l :: Symbol) i o a
   = forall xs. FsmMonad m =>
                  m (Map (AsMap ((l ':-> State m i) ': xs))) (Map (AsMap ((l ':-> State m o) ': xs))) a

type Remove m (l :: Symbol) s a
   = forall xs xs'. (FsmMonad m, Lookup xs l ~ Just (State m s), xs :\ l ~ xs') =>
                      m (Map xs) (Map xs') a

type None m a
   = forall (xs :: [Mapping Symbol *]). FsmMonad m => m (Map xs) (Map xs) a

-- Game

class FsmMonad m => Game m where
  spawn :: forall (l :: Symbol). Add m l Standing (Proxy l)
  jump :: forall (l :: Symbol). Proxy l -> Transition m l Standing Jumping ()
  land :: forall (l :: Symbol). Proxy l -> Transition m l Jumping Standing ()
  perish :: forall (l :: Symbol). Proxy l -> Remove m l Standing ()

data Standing
data Jumping
data Ducking

data GameImpl m i o a = GameImpl { runGameImpl :: m i o a }

data GameState s where
  Standing :: GameState Standing
  Jumping :: GameState Jumping
  Ducking :: GameState Ducking

type instance Combine (GameState s) (GameState s') = GameState s'

type instance LegalTransition Standing Standing = 'True
type instance LegalTransition Standing Jumping = 'True
type instance LegalTransition Jumping Standing = 'True

data Key = Up

data Event
  = Press Key

type instance Combine Int String = String
type instance Combine String Int = Int

test :: Game m => Add m l Standing (Proxy l)
test = spawn

test1 :: Game m => Proxy l -> Remove m l Standing ()
test1 = perish

-- test2 :: Game m => Add m l Standing (Proxy l)
-- test2 = test `fbind` \k -> freturn k
