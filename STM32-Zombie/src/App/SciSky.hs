----------------------------------------------------------------------------
-- |
-- Module      :  App.SciSky
-- Copyright   :  (c) Marc Fontaine 2025
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Test the Quantum SciSky flight controller for brushed quadrocopter.

module App.SciSky
where
import Control.Monad
import Control.Monad.IO.Class

import STM32.API
import STM32.GPIO as GPIO
import STM32.USART as USART
import App.Serial (sendCommDMA_Port)

ledGreen :: (Peripheral, Pin)
ledGreen = (GPIOB, Pin_3) --inverted

ledBlue :: (Peripheral, Pin)
ledBlue = (GPIOB, Pin_4) --inverted

motor_1 :: (Peripheral, Pin)
motor_1 = (GPIOA,Pin_8)

motor_2 :: (Peripheral, Pin)
motor_2 = (GPIOA,Pin_11)

motor_3 :: (Peripheral, Pin)
motor_3 = (GPIOB,Pin_6)

motor_4 :: (Peripheral, Pin)
motor_4 = (GPIOB,Pin_7)

motorPins :: [ (Peripheral, Pin) ]
motorPins = [ motor_1, motor_2, motor_3, motor_4 ]

allPins :: [ (Peripheral, Pin) ]
allPins =
    (map (\p -> (GPIOA, p)) [Pin_1 .. Pin_15])
 ++ (map (\p -> (GPIOB, p)) [Pin_1 .. Pin_15])
 ++ (map (\p -> (GPIOC, p)) [Pin_1 .. Pin_15])
 ++ (map (\p -> (GPIOD, p)) [Pin_1 .. Pin_15])

notMotorPins = filter (\p -> not $ p `elem` motorPins) allPins

blink :: MonadIO m => Wire -> STLT m ()
blink led = do
  let (port,_) = led
  peripheralClockOn port
  pinMode led $ GPOutPushPull MHz_2
  forM_ [1::Int .. 1] $ \i -> do
     liftIO $ print (i,led)
--     pinHigh led
--     delay 500000
--     pinLow led
     delay 500000
  liftIO $ do
    print led
    void getLine

allMotor :: MonadIO m => Bool -> STLT m ()
allMotor state =  forM_ motorPins $ \p -> do
  let (port,_) = p
  peripheralClockOn port
  pinMode p $ GPOutPushPull MHz_2
  pinOut p state

blinkTest :: [ (Peripheral, Pin) ] -> IO ()
blinkTest pins = runMI $ do
  initMI
  resetHalt
  forM_ pins $ \p -> do
    let (port,_) = p
    peripheralClockOn port
    pinMode p $ GPOutPushPull MHz_2
    pinLow p
--  forM_ allPins blink

testSerialOut :: IO ()
testSerialOut
  = runMI $ sendCommDMA_Port USART.stm32F103_UartPort1  (
    USART.defaultConfig {
      _baudRate   = BaudRateRegisterValue speed
    })
  where
    -- All Values between 893 and 995 work
    -- This runs at the clock speed that is set with `setDefaultClocks`
    speed_min = 893
    speed_max = 995
    speed = (speed_min + speed_max) `div` 2
