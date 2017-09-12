{-# LANGUAGE OverloadedStrings #-}
module Checkout.ADT where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List.NonEmpty
import           Text.Printf

import           Checkout
import qualified Checkout.PaymentProvider as PaymentProvider

-- PURE VERSION

-- start snippet states
data CheckoutState
  = NoItems
  | HasItems (NonEmpty CartItem)
  | NoCard (NonEmpty CartItem)
  | CardSelected (NonEmpty CartItem)
                 Card
  | CardConfirmed (NonEmpty CartItem)
                  Card
  | OrderPlaced
  deriving (Show, Eq)
-- end snippet states

-- start snippet events
data CheckoutEvent
  = Select CartItem
  | Checkout
  | SelectCard Card
  | Confirm
  | PlaceOrder
  | Cancel
  deriving (Show, Eq)
-- end snippet events

-- start snippet pure-fsm
type FSM s e =
  s -> e -> s
-- end snippet pure-fsm

-- start snippet checkout-signature
checkout :: FSM CheckoutState CheckoutEvent
-- end snippet checkout-signature
-- start snippet checkout-definition-select
checkout NoItems (Select item) =
  HasItems (item :| [])

checkout (HasItems items) (Select item) =
  HasItems (item <| items)
-- end snippet checkout-definition-select

checkout (HasItems items) Checkout =
  NoCard items

checkout (NoCard items) (SelectCard card) =
  CardSelected items card

checkout (CardSelected items card) Confirm =
  CardConfirmed items card

-- start snippet checkout-definition-cancel
checkout state Cancel =
  case state of
    NoCard items          -> HasItems items
    CardSelected items _  -> HasItems items
    CardConfirmed items _ -> HasItems items
    _                     -> state
-- end snippet checkout-definition-cancel

-- start snippet checkout-definition-place-order
checkout (CardConfirmed _items _card) PlaceOrder =
  -- Oh no, how do we actually do something?!
  OrderPlaced
-- end snippet checkout-definition-place-order

-- start snippet checkout-definition-unaccepted
checkout state _ = state
-- end snippet checkout-definition-unaccepted

runFSM :: FSM s e -> s -> [e] -> s
runFSM = foldl

testPureHappy :: Bool
testPureHappy =
  runFSM
    checkout
    NoItems
    [ Select "food"
    , Select "fish"
    , Checkout
    , SelectCard "visa"
    , Confirm
    , PlaceOrder
    ] ==
  OrderPlaced

testPureCancel :: Bool
testPureCancel =
  runFSM
    checkout
    NoItems
    [ Select "food"
    , Select "fish"
    , Checkout
    , SelectCard "visa"
    , Cancel
    ] ==
  HasItems ("fish" :| ["food"])

testPureInvalid :: Bool
testPureInvalid =
  runFSM
    checkout
    NoItems
    [Select "food", Select "fish", PlaceOrder] ==
  HasItems ("fish" :| ["food"])



-- IMPURE VERSION



-- start snippet impure-fsm
type ImpureFSM s e =
  s -> e -> IO s
-- end snippet impure-fsm

-- start snippet checkoutImpure-signature
checkoutImpure :: ImpureFSM CheckoutState CheckoutEvent
-- end snippet checkoutImpure-signature
-- start snippet checkoutImpure-definition-select
checkoutImpure NoItems (Select item) =
  return (HasItems (item :| []))

checkoutImpure (HasItems items) (Select item) =
  return (HasItems (item <| items))
-- end snippet checkoutImpure-definition-select

checkoutImpure (HasItems items) Checkout =
  return (NoCard items)

checkoutImpure (NoCard items) (SelectCard card) =
  return (CardSelected items card)

checkoutImpure (CardSelected items card) Confirm =
  return (CardConfirmed items card)

-- start snippet checkoutImpure-definition-cancel
checkoutImpure state Cancel =
  case state of
    NoCard items          -> return (HasItems items)
    CardSelected items _  -> return (HasItems items)
    CardConfirmed items _ -> return (HasItems items)
    _                     -> return state
-- end snippet checkoutImpure-definition-cancel

-- start snippet checkoutImpure-definition-place-order
checkoutImpure (CardConfirmed items card) PlaceOrder = do
  PaymentProvider.chargeCard card (calculatePrice items)
  return OrderPlaced
-- end snippet checkoutImpure-definition-place-order
  where
    -- dummy
    calculatePrice _ = 666 :: Price

-- start snippet checkoutImpure-definition-unaccepted
checkoutImpure state _ = return state
-- end snippet checkoutImpure-definition-unaccepted

-- start snippet runImpure
runImpure :: ImpureFSM s e -> s -> [e] -> IO s
runImpure = foldM
-- end snippet runImpure

-- start snippet withLogging
withLogging ::
     (Show s, Show e)
  => ImpureFSM s e
  -> ImpureFSM s e
withLogging fsm s e = do
  s' <- fsm s e
  liftIO $
    printf "- %s × %s → %s\n" (show s) (show e) (show s')
  return s'
-- end snippet withLogging

testImpureHappy :: IO Bool
testImpureHappy =
  (==) OrderPlaced <$>
-- start snippet runImpure-example
  runImpure
    (withLogging checkoutImpure)
    NoItems
    [ Select "food"
    , Select "fish"
    , Checkout
    , SelectCard "visa"
    , Confirm
    , PlaceOrder
    ]
-- end snippet runImpure-example

testImpureCancel :: IO Bool
testImpureCancel =
  (==) (HasItems ("fish" :| ["food"])) <$>
  runImpure
    (withLogging checkoutImpure)
    NoItems
    [ Select "food"
    , Select "fish"
    , Checkout
    , SelectCard "visa"
    , Cancel
    ]

testImpureInvalid :: IO Bool
testImpureInvalid =
  (==) (HasItems ("fish" :| ["food"])) <$>
  runImpure
    (withLogging checkoutImpure)
    NoItems
    [ Select "food"
    , Select "fish"
    , PlaceOrder
    ]
