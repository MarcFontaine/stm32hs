----------------------------------------------------------------------------
-- |
-- Module      :  App.TimerDMA
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This example show the combination of hardware timers with hardware DMA.
-- Timer 4 triggers DMA1_Channel7 and the DMA writes data to the USART.
-- Instead of the USART its also possible to write to any other peripheral.
-- Applications are wave-form-generation or hard-real-time control. 

module App.TimerDMA
where
import Control.Monad

import STM32.API
import qualified STM32.USART as USART
import STM32.DMA as DMA
import STM32.Timer as Timer

import Data.ByteString.Char8 as BS (pack)       

sendCommTimer :: IO ()
sendCommTimer
  = runMI $ sendCommTimer_Port USART.stm32F103_UartPort1  USART.defaultConfig

-- only works for USART.stm32F103_UartPort1 at the moment
sendCommTimer_Port :: USART.UartPort -> USART.Config -> MI ()
sendCommTimer_Port port config = do
  initMI
  resetHalt
  USART.configure port config

  let dmaBuffer = 0x20001000 
      dmaConfig = DMA.Config {
        _BufferSize         = 16
       ,_Direction          = PeripheralDST
       ,_MemoryBaseAddr     = dmaBuffer
       ,_MemoryDataSize     = Byte
       ,_MemoryInc          = True
       ,DMA._Mode           = Circular
       ,_PeripheralBaseAddr = regToAddr USART1 DR
       ,_PeripheralDataSize = Byte
       ,_PeripheralInc      = False
       ,_Priority           = Low
      }

  peripheralClockOn DMA1
  peripheralClockOn TIM4
  DMA.deInit DMA1_Channel7
  writeMem8 dmaBuffer $ BS.pack "abcdefghABCD123\n"
  
  DMA.disable DMA1_Channel7
  DMA.init DMA1_Channel7 dmaConfig
  DMA.enable DMA1_Channel7

  let timeBase = TimeBase {
     _Prescaler   = 7200  --  72 Mhz clock _Period counts in 0.1 ms
    ,_CounterMode = Down 
    ,_Period      = 10000  -- 1s
    ,_ClockDevision = CKD_DIV1
    ,_RepetitionCounter  =0
    }

  Timer.deInit TIM4
  Timer.timeBaseInit TIM4 timeBase 
  bitReset TIM4 CR1_URS
  bitSet TIM4 DIER_UDE
  bitSet TIM4 CR1_CEN
