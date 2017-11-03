----------------------------------------------------------------------------
-- |
-- Module      :  App.Stepper
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Stepper motor control. (GPIO bit-banging)
-- This example uses bit-banging to toggle two GPIO pins.
-- It does not show a special STM32 feature.
-- (But I just wanted to test a stepper motor) 

module App.Stepper
where
import Control.Monad

import STM32.API
import STM32.GPIO as GPIO

data Direction = CW | CCW deriving (Show,Eq)

-- | Rotatet the stepper motor a number of steps clock wise or conter clock wise.
runStepper:: Direction -> Int -> IO ()
runStepper = runStepperDelay 25000

runStepperDelay :: Int -> Direction -> Int -> IO ()
runStepperDelay pause dir steps = runMI $ do
  let
      port = GPIOB
      dirWire  = (port,Pin_2)
      stepWire = (port,Pin_1)
  initMI
  resetHalt
  peripheralClockOn port
  pinMode dirWire $ GPOutPushPull MHz_2
  pinMode stepWire $ GPOutPushPull MHz_2
  case dir of
    CW  -> pinHigh dirWire
    CCW -> pinLow dirWire
  replicateM_ steps $ do
     delay pause
     pinHigh stepWire
     delay pause
     pinLow stepWire
