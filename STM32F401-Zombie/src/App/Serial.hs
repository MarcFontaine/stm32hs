----------------------------------------------------------------------------
-- |
-- Module      :  App.Serial
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Serial port output.
--

module App.Serial
where
import Control.Monad

import STM32.API
import qualified STM32.USART as USART
import STM32.DMA as DMA

import Data.ByteString.Char8 as BS (pack)

-- | Send some chars one after the other
sendComm :: IO ()
sendComm
  = runMI $ sendComm_Port USART.stm32F103_UartPort1  USART.defaultConfig

sendComm_Port :: USART.UartPort -> USART.Config -> MI ()
sendComm_Port port config = do
  initMI
  resetHalt
  setDefaultClocks
  USART.configure port config
  forever $ do
     forM_ [65..90] $ USART.sendWord8 $ USART._UartPeripheral port
     delay 500000

-- | Send a block chars using DMA.
sendCommDMA :: IO ()
sendCommDMA
  = runMI $ sendCommDMA_Port USART.stm32F103_UartPort1  USART.defaultConfig

-- only works for USART.stm32F103_UartPort1 at the moment
sendCommDMA_Port :: USART.UartPort -> USART.Config -> MI ()
sendCommDMA_Port port config = do
  initMI
  resetHalt
  setDefaultClocks
  USART.configure port config

  let dmaBuffer = 0x20001000
      dmaConfig = DMA.Config {
        _BufferSize         = 16
       ,_Direction          = PeripheralDST
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
  bitSet USART1 CR3_DMAT
  DMA.deInit DMA1_Channel4
  writeMem8 dmaBuffer $ BS.pack "abcdefghABCD123\n"

  forever $ do
     DMA.disable DMA1_Channel4
     DMA.init DMA1_Channel4 dmaConfig
     DMA.enable DMA1_Channel4
     delay 500000

