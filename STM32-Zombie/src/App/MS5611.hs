{-- todo work in progress I2C not implemented yet -}
module App.MS5611
where

import Control.Monad
import Control.Monad.IO.Class

import STM32.I2C as I2C
import qualified STM32.RTC as RTC
import qualified STM32.RCC as RCC
import qualified STM32.GPIO as GPIO
import Device
import STM32.MachineInterface
import STM32.Utils

barometer :: IO ()
barometer = runMI $ do
  initMI
  resetHalt
  RTC.getCounter >>= print'

  let led = (GPIOB,GPIO.Pin_15)
  RCC.peripheralClockOn GPIOB
  GPIO.pinMode led $ GPIO.GPOutPushPull GPIO.Mhz_2
  replicateM_ 3 $ do
     GPIO.pinHigh led
     delay 500000
     GPIO.pinLow led
     delay 500000


