----------------------------------------------------------------------------
-- |
-- Module      :  App.Blink
-- Copyright   :  (c) Marc Fontaine 2017-2025
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The HelloWorld of microcontroller programming: A blinking LED.

module App.Blink
where
import           Control.Monad
import           Control.Monad.IO.Class

import           F401.API
import           F401.GPIO              as GPIO

blink :: IO ()
blink = runMI $ blinkLED (GPIOC, Pin_13)

blinkLED :: MonadIO m => Wire -> STLT m ()
blinkLED led = do
  initMI
  resetHalt
--  let (port,_) = led
--  peripheralClockOn port
  pinMode led $ GPOutPushPull LowSpeed
  forever $ do
     pinHigh led
     delay 500000
     pinLow led
     delay 500000
