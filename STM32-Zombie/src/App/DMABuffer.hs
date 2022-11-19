----------------------------------------------------------------------------
-- |
-- Module      :  App.DMABuffer
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- In this example, the controller reads chars from the USART
-- and writes them to a RAM buffer using DMA.

{-# LANGUAGE OverloadedStrings #-}
module App.DMABuffer
where
import Control.Monad
import Control.Monad.IO.Class

import STM32.API
import qualified STM32.USART as USART
import STM32.DMA as DMA
import STM32.GPIO as GPIO
import qualified Data.ByteString as BS
import Data.ByteString.Char8 as BSC (putStrLn)
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import Data.Binary
import Data.Binary.Get
import Data.Char (chr,isPrint)


-- | Initialize the Hardware and keep polling the DMA Buffer.
-- This function loops for ever.
-- Though after the buffer is full nothing interesting happens.
readCommDMA :: IO ()
readCommDMA = runMI $ do
  initMI
  resetHalt
  setDefaultClocks
  USART.deInit USART1
  peripheralClockOn USART1
  peripheralClockOn GPIOA
  peripheralClockOn AFIO

  GPIO.pinMode (GPIOA,Pin_9) (AlternateOutPushPull MHz_2)
  GPIO.pinMode (GPIOA,Pin_10) InputPullUp

  USART.enable USART1
  USART.init USART1 USART.defaultConfig
  bitSet USART1 CR1_RE

  let dmaBuffer = 0x20001000
      dmaConfig = DMA.Config {
        _BufferSize         = 16
       ,_Direction          = PeripheralSRC
       ,_MemoryBaseAddr     = dmaBuffer
       ,_MemoryDataSize     = Byte
       ,_MemoryInc          = True
       ,DMA._Mode           = Normal
       ,_PeripheralBaseAddr = regToAddr USART1 DR
       ,_PeripheralDataSize = Byte
       ,_PeripheralInc      = False
       ,_Priority           = Low
      }

  peripheralClockOn DMA1
  bitSet USART1 CR3_DMAR
  DMA.deInit DMA1_Channel5
  DMA.disable DMA1_Channel5
  DMA.init DMA1_Channel5 dmaConfig
  DMA.enable DMA1_Channel5
  bitSet USART1 CR3_DMAR
  writeMem8 dmaBuffer "XXXXXXXXXXXXXXXX"
  forever $ do
    buffer <- readMem8 dmaBuffer 16
    liftIO $ BSC.putStrLn buffer
    delay 500000


-- | Initialize the Hardware and keep polling the DMA Buffer.
-- 'uartRingBuffer' uses a ring buffer that wraps over when filled up.
-- The DMA controller is configured to read Bytes (8 Bit) from the UART
-- and write half words (16 Bit) to then RAM. This means
-- it transfers a char and clears out the next byte to flag that this position
-- in the buffer has been written.


uartRingBuffer :: IO ()
uartRingBuffer = runMI $ do
  initMI
  resetHalt
  setDefaultClocks
  USART.deInit USART1
  peripheralClockOn USART1
  peripheralClockOn GPIOA
  peripheralClockOn AFIO

  GPIO.pinMode (GPIOA,Pin_9) (AlternateOutPushPull MHz_2)
  GPIO.pinMode (GPIOA,Pin_10) InputPullUp

  USART.enable USART1
  USART.init USART1 USART.defaultConfig
  bitSet USART1 CR1_RE

  let
      entries :: Num a => a
      entries = 20
      bufferSize :: Num a => a
      bufferSize = 2 * entries
  let dmaBuffer = 0x20001000
      dmaConfig = DMA.Config {
        _BufferSize         = entries
       ,_Direction          = PeripheralSRC
       ,_MemoryBaseAddr     = dmaBuffer
       ,_MemoryDataSize     = HalfWord
       ,_MemoryInc          = True
       ,DMA._Mode           = Circular
       ,_PeripheralBaseAddr = regToAddr USART1 DR
       ,_PeripheralDataSize = Byte
       ,_PeripheralInc      = False
       ,_Priority           = Low
      }

  peripheralClockOn DMA1
  bitSet USART1 CR3_DMAR
  DMA.deInit DMA1_Channel5
  DMA.disable DMA1_Channel5
  DMA.init DMA1_Channel5 dmaConfig
  DMA.enable DMA1_Channel5
  bitSet USART1 CR3_DMAR
  forever $ do
    buffer <- readMem8 dmaBuffer bufferSize
    writeMem8 dmaBuffer $ BS.replicate bufferSize 1
    let slots = runGet (replicateM entries parseSlot)
                          $ BSL.fromStrict buffer

    liftIO $ print (map fst slots,map (\(_,x) -> if x then 'X' else ' ') slots)
    delay 500000

parseSlot :: Get (Char, Bool)
parseSlot =do
  c <- fmap (chr .fromIntegral) getWord8
  f <- getWord8
  return (if isPrint c then c else ' ', f==0)
