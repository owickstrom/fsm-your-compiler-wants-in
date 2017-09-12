module Checkout

import Data.Vect
import Control.ST
import Effects
import Effect.Random

-- start snippet Protocol
namespace Protocol

  Item : Type
  Item = String

  Items : Nat -> Type
  Items n = Vect n Item

  Card : Type
  Card = String

  OrderId : Type
  OrderId = String
-- end snippet Protocol

-- start snippet CheckoutState
  data CheckoutState
    = HasItems Nat
    | NoCard Nat
    | CardEntered Nat
    | CardConfirmed Nat
    | OrderPlaced
-- end snippet CheckoutState

  partial
  cancelTransition : CheckoutState -> CheckoutState
  cancelTransition s =
    case s of
      NoCard n => HasItems n
      CardEntered n => HasItems n
      CardConfirmed n => HasItems n
      s => s

-- start snippet CancelState
  data CancelState : CheckoutState -> (n : Nat) -> Type where

    NoCardCancel : CancelState (NoCard n) n

    CardEnteredCancel : CancelState (CardEntered n) n

    CardConfirmedCancel : CancelState (CardConfirmed n) n
-- end snippet CancelState

-- start snippet Checkout
  interface Checkout (m : Type -> Type) where
    State : CheckoutState -> Type
-- end snippet Checkout
-- start snippet Checkout-initial
    initial
      : ST m Var [add (State (HasItems 0))]
-- end snippet Checkout-initial
-- start snippet Checkout-select
    select
      : (c : Var)
      -> Item
      -> ST m () [c ::: State (HasItems n)
                        :-> State (HasItems (S n))]
-- end snippet Checkout-select
-- start snippet Checkout-checkout
    checkout
      : (c : Var)
      -> ST m () [c ::: State (HasItems (S n))
                        :-> State (NoCard (S n))]
-- end snippet Checkout-checkout
    selectCard : (c : Var) -> Card -> ST m () [c ::: State (NoCard n)
                                               :-> State (CardEntered n)]
    confirm : (c : Var) -> ST m () [c ::: State (CardEntered n)
                                    :-> State (CardConfirmed n)]
    placeOrder : (c : Var) -> ST m () [c ::: State (CardConfirmed n)
                                             :-> State OrderPlaced]
-- start snippet Checkout-cancel
    cancel
      : (c : Var)
      -> { auto prf : CancelState s n }
      -> ST m () [c ::: State s
                        :-> State (HasItems n)]
-- end snippet Checkout-cancel
    end : (c : Var) -> ST m OrderId [remove c (State OrderPlaced)]

namespace Implementation

-- start snippet Implementation-CheckoutState
  data CheckoutState : Protocol.CheckoutState -> Type where

    HasItems : Items n -> CheckoutState (HasItems n)

    NoCard : Items n -> CheckoutState (NoCard n)

    CardEntered
      : Items n -> Card -> CheckoutState (CardEntered n)

    CardConfirmed
      : Items n -> Card -> CheckoutState (CardConfirmed n)

    OrderPlaced : OrderId -> CheckoutState OrderPlaced
-- end snippet Implementation-CheckoutState

-- start snippet Implementation-Checkout
  implementation (Monad m, ConsoleIO m) => Checkout m where
    State = Control.ST.State . Implementation.CheckoutState
-- end snippet Implementation-Checkout

    initial = new (HasItems Nil)

    select co item = do
      case !(read co) of
        HasItems items =>
          write co (HasItems (item :: items))

    checkout co = do
      HasItems items <- read co
      write co (NoCard items)

    selectCard co card = do
      NoCard items <- read co
      write co (CardEntered items card)

    confirm co = do
      CardEntered items card <- read co
      write co (CardConfirmed items card)

    placeOrder co = do
      CardConfirmed items card <- read co
      n <- lift (Effects.run (rndInt 1 1000))
      let orderId = ("order-" ++ cast n)
      putStrLn ("Order '" ++ orderId ++ "' placed.")
      write co (OrderPlaced orderId)

    end co = do
      OrderPlaced orderId <- read co
      delete co
      pure orderId

    cancel co {prf = NoCardCancel} = do
      putStrLn "Cancelling checkout directly."
      NoCard items <- read co
      write co (HasItems items)

    cancel co {prf = CardEnteredCancel} = do
      putStrLn "Cancelling checkout after having entered card."
      CardEntered items card <- read co
      write co (HasItems items)

    cancel co {prf = CardConfirmedCancel} = do
      putStrLn "Cancelling checkout after having confirmed card."
      CardConfirmed items card <- read co
      write co (HasItems items)

using (ConsoleIO m, Checkout m)

  orElse : a -> a -> Bool -> a
  orElse x y c = if c then x else y

  continueOrCancel
    : (c : Var)
    -> { auto prf : CancelState s n }
    -> STrans m Bool [c ::: State {m} s]
       ([c ::: State {m} s]
        `orElse`
        [c ::: State {m} (HasItems n)])
  continueOrCancel c {prf} = do
    Control.ST.putStrLn "Continue? (y/n)"
    case !(getStr) of
      "y" => pure True
      _ => do
        cancel c {prf=prf}
        pure False

  mutual
-- start snippet selectMore
    total
    selectMore
      : (c : Var)
      -> ST m () [c ::: State {m} (HasItems n)
                        :-> State {m} (HasItems (S n))]
    selectMore c {n} = do
      if n == 0
        then putStrLn "What do you want to add?"
        else putStrLn "What more do you want to add?"
      item <- getStr
      select c item
-- end snippet selectMore

-- start snippet checkoutWithItems
    total
    checkoutWithItems
      : (c : Var)
      -> ST m Bool [c ::: State {m} (HasItems (S n))
                          :-> (State {m} OrderPlaced
                               `orElse`
                               State {m} (HasItems (S n)))]
    checkoutWithItems c = do
      checkout c
      True <- continueOrCancel c | False => pure False
      putStrLn "Enter your card:"
      selectCard c !getStr
      True <- continueOrCancel c | False => pure False
      confirm c
      True <- continueOrCancel c | False => pure False
      placeOrder c
      pure True
-- end snippet checkoutWithItems

-- start snippet checkoutOrShop
    total
    checkoutOrShop
      : (c : Var)
      -> STLoop m () [remove c (State {m} (HasItems (S n)))]
    checkoutOrShop c = do
      True <- checkoutWithItems c | False => goShopping c
      orderId <- end c
      putStrLn ("Checkout complete with order ID: " ++ orderId)
      pure ()
-- end snippet checkoutOrShop

-- start snippet goShopping
    total
    goShopping
      : (c : Var)
      -> STLoop m () [remove c (State {m} (HasItems n))]
    goShopping c = do
      selectMore c
      putStrLn "Checkout? (y/n)"
      case !getStr of
        "y" => checkoutOrShop c
        _ => goShopping c
-- end snippet goShopping

-- start snippet program
  total
  program : STransLoop m () [] (const [])
  program = do
    c <- initial
    goShopping c
-- end snippet program

export
-- start snippet runCheckout
runCheckout : IO ()
runCheckout =
  runLoop forever program (putStrLn "Oops.")
-- end snippet runCheckout
