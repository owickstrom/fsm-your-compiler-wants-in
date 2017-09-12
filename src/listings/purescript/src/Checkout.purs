module Checkout where

import Prelude

import Control.IxMonad (ibind, ipure)
import Control.IxMonad.Leffe (class IxMonadLeffe, Leffe, addResource, getResource, modifyResource, removeResource, replaceResource, runLeffe)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Plus (empty)
import Data.Array ((:))
import Data.NonEmpty (NonEmpty, (:|))
import Data.Symbol (class IsSymbol)
import FSM (Add, Remove, Transition, log)
import Type.Data.Symbol (SProxy(..))

type Item = String
type Items = NonEmpty Array Item

type Card = String
type OrderId = String

-- States

data NoItems = NoItems

data HasItems = HasItems Items

data NoCard = NoCard Items

data CardSelected = CardSelected Items Card

data CardConfirmed = CardConfirmed Items Card

data OrderPlaced = OrderPlaced OrderId

initial :: forall m co. SProxy co -> Add m co NoItems Unit
initial co = addResource co NoItems

class CheckoutSelect ri ro from | from -> ri, from -> ro where
  select :: forall m r label
     . IxMonadLeffe m
    => IsSymbol label
    => RowCons label from r ri
    => RowCons label HasItems r ro
    => SProxy label -> Item -> m {|ri} {|ro} Unit

instance checkoutSelectNoItems :: CheckoutSelect ri ro NoItems where
  select co item = modifyResource co addItem
    where
      addItem NoItems = HasItems (item :| empty)

instance checkoutSelectHasItems :: CheckoutSelect r r HasItems where
  select co item = modifyResource co addItem
    where
      addItem (HasItems (second :| rest)) = HasItems (item :| second : rest)

checkout
  :: forall m co
   . SProxy co
  -> Transition m co HasItems NoCard Unit
checkout co =
  modifyResource co (\(HasItems items) -> NoCard items)

selectCard
  :: forall m co
   . SProxy co
  -> Card
  -> Transition m co NoCard CardSelected Unit
selectCard co card =
  modifyResource co (\(NoCard items) -> CardSelected items card)

confirmCard
  :: forall m co
   . SProxy co
  -> Transition m co CardSelected CardConfirmed Unit
confirmCard co =
  modifyResource co (\(CardSelected items card) -> CardConfirmed items card)

placeOrder
  :: forall m e co r
   . SProxy co
  -> Transition m co CardConfirmed OrderPlaced Unit
placeOrder co = do
  CardConfirmed items card <- getResource co
  let orderId = "order-123"
  replaceResource co (OrderPlaced orderId)
  where
    bind = ibind
    discard = ibind

end :: forall m co. SProxy co -> Remove m co OrderPlaced OrderId
end co = do
  OrderPlaced orderId <- getResource co
  removeResource co
  ipure orderId
  where
    bind = ibind
    discard = ibind

checkoutFlow
  :: forall m e
   . MonadEff (console :: CONSOLE | e) m
  => Leffe m {} {} Unit
checkoutFlow = do
  let co = SProxy :: SProxy "my-checkout"
  initial co
  select co "Fish"
  select co "Banana"
  checkout co
  let card = "visa"
  selectCard co card
  log ("Use '" <> card <> "'?")
  confirmCard co
  placeOrder co
  orderId <- end co
  log ("Order placed: " <> orderId)

  where
    bind = ibind
    discard = ibind

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = runLeffe checkoutFlow
