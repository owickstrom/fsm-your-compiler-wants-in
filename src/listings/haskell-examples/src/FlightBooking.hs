{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
module FlightBooking where

import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Data.Text                   (Text)
import           Prelude                     hiding (log)
import qualified System.Timeout.Lifted       as Timeout

import qualified Prompt

type FlightNumber = Text
type Seat = Text

-- start snippet states
data FlightSelected

data SeatReserved

data SeatBooked

data BookingCancelled

data SeatReleased
-- end snippet states

-- start snippet multi-states
data CancelState m
   = SeatReleasedCancel (State m SeatReleased)
   | SeatReservedCancel (State m SeatReserved)

data EndState m
   = CancelledEnd (State m BookingCancelled)
   | BookedEnd (State m SeatBooked)
-- end snippet multi-states

-- start snippet flight-booking-class
class FlightBooking m where
  type State m :: * -> *
  selectFlight :: FlightNumber -> m (State m FlightSelected)
  selectSeat ::
       State m FlightSelected
    -> Seat
    -> m (State m SeatReserved)
  payForSeat ::
       State m SeatReserved -> m (State m SeatBooked)
  timeout ::
       State m SeatReserved -> m (State m SeatReleased)
  restart ::
       State m SeatReleased -> m (State m FlightSelected)
  cancel :: CancelState m -> m (State m BookingCancelled)
  end :: EndState m -> m ()
-- end snippet flight-booking-class

log :: MonadBaseControl IO m => String -> m ()
log = liftBase . putStrLn

prompt :: MonadBaseControl IO m => Text -> m Text
prompt = liftBase . Prompt.prompt

confirmPrompt :: MonadBaseControl IO m => Text -> m Bool
confirmPrompt = liftBase . Prompt.confirmPrompt

seconds :: Int -> Int
seconds = (*) 1000000

minutes :: Int -> Int
minutes = (*) 60 . seconds

-- start snippet flight-selected-start
withFlightSelected ::
     (MonadBaseControl IO m, Monad m, FlightBooking m)
  => State m FlightSelected -> m ()
withFlightSelected flightSelected = do
  seatReserved <- prompt "Seat:" >>= selectSeat flightSelected
  answer <- Timeout.timeout (minutes 5) (confirmPrompt "Buy?")
  case answer of
-- end snippet flight-selected-start
-- start snippet flight-selected-booked
    Just True -> do
      seatBooked <- payForSeat seatReserved
      log "Seat booked!"
      end (BookedEnd seatBooked)
-- end snippet flight-selected-booked
-- start snippet flight-selected-cancelled
    Just False -> do
      cancelled <- cancel (SeatReservedCancel seatReserved)
      log "Your booking has been cancelled."
      end (CancelledEnd cancelled)
-- end snippet flight-selected-cancelled
-- start snippet flight-selected-timeout
    Nothing -> do
      released <- timeout seatReserved
      wantsRestart <- confirmPrompt "Restart?"
      if wantsRestart
        then restart released >>= withFlightSelected
        else do
          cancelled <- cancel (SeatReleasedCancel released)
          end (CancelledEnd cancelled)
-- end snippet flight-selected-timeout

-- start snippet book-flight
bookFlight ::
     (MonadBaseControl IO m, Monad m, FlightBooking m)
  => m ()
bookFlight =
  prompt "Flight number:"
  >>= selectFlight
  >>= withFlightSelected
-- end snippet book-flight
