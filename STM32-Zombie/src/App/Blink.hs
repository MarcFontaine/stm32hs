----------------------------------------------------------------------------
-- |
-- Module      :  App.Blink
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The HelloWorld of microcontroller programming: A blinking LED.

module App.Blink
where
import Control.Monad

import STM32.API
import STM32.GPIO as GPIO

blink :: IO ()
blink = runMI $ blinkLED (GPIOC,Pin_13)

blinkLED :: Wire -> MI ()
blinkLED led = do
  initMI
  resetHalt
  let (port,_) = led
  peripheralClockOn port
  pinMode led $ GPOutPushPull MHz_2
  forever $ do
     pinHigh led
     delay 500000
     pinLow led
     delay 500000
