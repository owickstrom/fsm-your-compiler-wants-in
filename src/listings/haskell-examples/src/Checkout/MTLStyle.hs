{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Checkout.MTLStyle where

import           Control.Monad.IO.Class
import           Data.List.NonEmpty
import           Data.Semigroup
import qualified Data.Text.IO             as T

import           Checkout
import qualified Checkout.PaymentProvider as PaymentProvider
import           Prompt


-- * States


-- start snippet states
data NoItems

data HasItems

data NoCard

data CardSelected

data CardConfirmed

data OrderPlaced
-- end snippet states

-- start snippet select-state
data SelectState m
  = NoItemsSelect (State m NoItems)
  | HasItemsSelect (State m HasItems)
-- end snippet select-state

-- start snippet cancel-state
data CancelState m
  = NoCardCancel (State m NoCard)
  | CardSelectedCancel (State m CardSelected)
  | CardConfirmedCancel (State m CardConfirmed)
-- end snippet cancel-state


-- * State Machine Transitions


-- start snippet state-class
class Checkout m where
  type State m :: * -> *
-- end snippet state-class
-- start snippet state-class-initial
  initial :: m (State m NoItems)
-- end snippet state-class-initial
-- start snippet state-class-select
  select ::
       SelectState m
    -> CartItem
    -> m (State m HasItems)
-- end snippet state-class-select
-- start snippet state-class-checkout
  checkout :: State m HasItems -> m (State m NoCard)
-- end snippet state-class-checkout
-- start snippet state-class-selectCard
  selectCard ::
       State m NoCard -> Card -> m (State m CardSelected)
-- end snippet state-class-selectCard
-- start snippet state-class-confirm
  confirm ::
       State m CardSelected -> m (State m CardConfirmed)
-- end snippet state-class-confirm
-- start snippet state-class-placeOrder
  placeOrder ::
       State m CardConfirmed -> m (State m OrderPlaced)
-- end snippet state-class-placeOrder
-- start snippet state-class-cancel
  cancel :: CancelState m -> m (State m HasItems)
-- end snippet state-class-cancel
-- start snippet state-class-end
  end :: State m OrderPlaced -> m OrderId
-- end snippet state-class-end


-- * State Machine Program


calculateTotal :: [CartItem] -> Price
calculateTotal _ = 666

-- start snippet fillCart
fillCart ::
     (Checkout m, MonadIO m)
  => State m NoItems
  -> m (State m HasItems)
fillCart noItems = do
  first <- prompt "First item:"
  select (NoItemsSelect noItems) first >>= selectMoreItems
-- end snippet fillCart

-- start snippet selectMoreItems
selectMoreItems ::
     (Checkout m, MonadIO m)
  => State m HasItems
  -> m (State m HasItems)
selectMoreItems s = do
  more <- confirmPrompt "More items?"
  if more
    then prompt "Next item:" >>=
         select (HasItemsSelect s) >>=
         selectMoreItems
    else return s
-- end snippet selectMoreItems

-- start snippet startCheckout
startCheckout ::
     (Checkout m, MonadIO m)
  => State m HasItems
  -> m (State m OrderPlaced)
startCheckout hasItems = do
  noCard <- checkout hasItems
  card <- prompt "Card:"
  cardSelected <- selectCard noCard card
  useCard <-
    confirmPrompt ("Confirm use of '" <> card <> "'?")
  if useCard
    then confirm cardSelected >>= placeOrder
    else cancel (CardSelectedCancel cardSelected) >>=
         selectMoreItems >>=
         startCheckout
-- end snippet startCheckout

-- start snippet checkoutProgram
checkoutProgram ::
     (Checkout m, MonadIO m)
  => m OrderId
checkoutProgram =
  initial >>= fillCart >>= startCheckout >>= end
-- end snippet checkoutProgram


-- * Implementation


-- start snippet CheckoutT
newtype CheckoutT m a = CheckoutT
  { runCheckoutT :: m a
  } deriving ( Monad
             , Functor
             , Applicative
             , MonadIO
             )
-- end snippet CheckoutT

-- start snippet CheckoutState
data CheckoutState s where
  NoItems :: CheckoutState NoItems

  HasItems :: NonEmpty CartItem -> CheckoutState HasItems

  NoCard :: NonEmpty CartItem -> CheckoutState NoCard

  CardSelected
    :: NonEmpty CartItem
    -> Card
    -> CheckoutState CardSelected

  CardConfirmed
    :: NonEmpty CartItem
    -> Card
    -> CheckoutState CardConfirmed

  OrderPlaced :: OrderId -> CheckoutState OrderPlaced
-- end snippet CheckoutState

calculatePrice :: NonEmpty CartItem -> Price
calculatePrice = const 666

newOrderId :: MonadIO m => m OrderId
newOrderId = liftIO (return "foo")

-- start snippet instance-head
instance (MonadIO m) => Checkout (CheckoutT m) where
  type State (CheckoutT m) = CheckoutState
-- end snippet instance-head
-- start snippet instance-initial
  initial = return NoItems
-- end snippet instance-initial
-- start snippet instance-select
  select state item =
    case state of
      NoItemsSelect NoItems ->
        return (HasItems (item :| []))
      HasItemsSelect (HasItems items) ->
        return (HasItems (item <| items))
-- end snippet instance-select
  checkout (HasItems items) = return (NoCard items)
  selectCard (NoCard items) card =
    return (CardSelected items card)
  confirm (CardSelected items card) =
    return (CardConfirmed items card)
-- start snippet instance-placeOrder
  placeOrder (CardConfirmed items card) = do
    orderId <- newOrderId
    let price = calculatePrice items
    PaymentProvider.chargeCard card price
    return (OrderPlaced orderId)
-- end snippet instance-placeOrder
  cancel cancelState =
    case cancelState of
      NoCardCancel (NoCard items) -> return (HasItems items)
      CardSelectedCancel (CardSelected items _) ->
        return (HasItems items)
      CardConfirmedCancel (CardConfirmed items _) ->
        return (HasItems items)
  end (OrderPlaced orderId) = return orderId
-- end snippet instance

-- start snippet example
example :: IO ()
example = do
  orderId <- runCheckoutT checkoutProgram
  T.putStrLn ("Completed with order ID: " <> orderId)
-- end snippet example
