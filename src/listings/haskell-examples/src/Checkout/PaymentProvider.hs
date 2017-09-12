{-# LANGUAGE OverloadedStrings #-}
-- | Dummy module for this presentation.

module Checkout.PaymentProvider where

import           Control.Monad.IO.Class
import           Data.Semigroup
import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import           Checkout

chargeCard :: MonadIO m => Card -> Price -> m ()
chargeCard card price =
  liftIO
    (T.putStrLn
       ("Charging card " <> card <> " $" <>
        T.pack (show price)))
