----------------------------------------------------------------------------
-- |
-- Module      :  App.TLC5947
-- Copyright   :  (c) Marc Fontaine 2019
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- LED animation using a TLC5947 controller chip (bit-banging based) .
-- TODO: Add DMA/SPI based implementation.

module App.TLC5947
where
import Prelude hiding (sin)
import Control.Monad
import Data.Bits
import Data.List

import STM32.API
import STM32.GPIO as GPIO

main :: IO ()
main = animateBitBang

-- Wiring of the CircuitHup Haskell logo PCB.
blank :: Wire
xlat  :: Wire
sin   :: Wire
sclk  :: Wire
blank = (GPIOB, Pin_7)
xlat  = (GPIOB, Pin_6)
sin   = (GPIOB, Pin_5)
sclk  = (GPIOB, Pin_3)

ledsOff :: IO ()
ledsOff = runMI $ do
  initMI
  resetHalt
  let (port,_) = blank
  peripheralClockOn port
  pinMode blank $ GPOutPushPull MHz_2
  pinHigh blank

animateBitBang :: IO ()
animateBitBang = runMI $ do
  initMI
  resetHalt
  let (port,_) = blank
  peripheralClockOn port
  pinMode blank $ GPOutPushPull MHz_2
  pinMode xlat  $ GPOutPushPull MHz_2
  pinMode sclk  $ GPOutPushPull MHz_2
  pinMode sin   $ GPOutPushPull MHz_2
  pinLow blank
  forever $ do
    forM_ animation $ \leds -> do
      forM_ leds shiftInCurrent
      togglePin xlat

animation :: [[Word16]]
animation = take 24 $ map (take 24) $ tails $ cycle (4095: replicate 23 0)

togglePin :: Wire -> MI ()
togglePin w = do
  pinHigh w
  pinLow w

shiftInCurrent :: Word16 -> MI ()
shiftInCurrent i
  = forM_ [0..11] $ \b -> do
       if testBit i b then pinHigh sin else pinLow sin
       togglePin sclk
