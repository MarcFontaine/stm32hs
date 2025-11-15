----------------------------------------------------------------------------
-- |
-- Module      :  App.ADC
-- Copyright   :  (c) Marc Fontaine 2017-2022
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module shows an example for using the analog digital converter.
-- The ADC of the STM32 works best in combination with DMA transfers.
-- This example turns the STM32 into a small digital storage oscilloscope.
-- Thanks to DMA transfers, one can sample with precise timings
-- and sampling rate is not limited by the speed of the Haskell code.

module App.ADC
where
import Control.Monad
import Control.Monad.IO.Class

import STM32.API
import STM32.DMA as DMA
import STM32.GPIO as GPIO
import STM32.ADC as ADC

import qualified Data.ByteString.Lazy as BSL (fromStrict)
import Data.Binary.Get

-- this is buggy the channels get mixed up from time to time
-- maybe DMA out of sync
adc3channel :: IO ()
adc3channel = runMI $ do
  initMI
  resetHalt
  setDefaultClocks

  peripheralClockOn GPIOA
  GPIO.pinMode (GPIOA,Pin_1) InputAnalog
  GPIO.pinMode (GPIOA,Pin_3) InputAnalog
  GPIO.pinMode (GPIOA,Pin_5) InputAnalog

  let overSampling :: Num x => x
      overSampling = 8
      bufferSize :: Num x => x
      bufferSize = overSampling * 2 *3
      dmaCount = overSampling *3
  let dmaBuffer = 0x20001000
      dmaConfig = DMA.Config {
        _BufferSize         = dmaCount
       ,_Direction          = PeripheralSRC
       ,_MemoryBaseAddr     = dmaBuffer
       ,_MemoryDataSize     = HalfWord
       ,_MemoryInc          = True
       ,DMA._Mode           = Circular
       ,_PeripheralBaseAddr = regToAddr ADC1 DR
       ,_PeripheralDataSize = HalfWord
       ,_PeripheralInc      = False
       ,_Priority           = High
      }

  peripheralClockOn DMA1
  DMA.deInit DMA1_Channel1
  DMA.init DMA1_Channel1 dmaConfig
  DMA.enable DMA1_Channel1

  let adcConfig = ADC.Config {
     ADC._Mode           =  Independent
    ,_ScanConvMode       = True
    ,_ContinuousConvMode = True
    ,_ExternalTrigConv   = ExternalTrigConv_None
    ,_DataAlign          = AlignRight
    ,_NbrOfChannel       = 3
    }
  peripheralClockOn ADC1
  ADC.init ADC1 adcConfig

  ADC.regularChannelConfig ADC1 Channel_1 1 SampleTime_71Cycles5
  ADC.regularChannelConfig ADC1 Channel_3 2 SampleTime_71Cycles5
  ADC.regularChannelConfig ADC1 Channel_5 3 SampleTime_71Cycles5

  ADC.dmaCmd ADC1 True
  ADC.cmd ADC1 True
  -- todo : implement calibration
  ADC.softwareStartConvCmd ADC1 True

  forever $ do
    buffer <- readMem8 dmaBuffer bufferSize
    let
       vals :: [(Word16,Word16,Word16)]
       vals = runGet
               ( replicateM overSampling
                 ((,,) <$> getWord16le  <*> getWord16le  <*> getWord16le)
               )
               (BSL.fromStrict buffer)
       average sel = fromIntegral (sum $ map sel vals) * 100 `div` overSampling
       w1 :: Int
       w1 = average (\(x,_,_) -> x)
       w2 :: Int
       w2 = average (\(_,x,_) -> x)
       w3 :: Int
       w3 = average (\(_,_,x) -> x)
{-
   when some input pin is connect to a poti while some
   neighboring inputs are left floating
   the floating ones do not "float" randomely
   floating inputs are pulled by the poti
-}
    print' (w1,w2,w3)
    delay 100000

-- | Periodically sample a block of data and write it to a file.
-- In combination with a wave-form viewer that can detect file updates,
-- this works as a poor mans' digital storage oscilloscope.
sampleBlock :: FilePath -> IO ()
sampleBlock filename = runMI $ do
  initMI
  resetHalt
  setDefaultClocks

  peripheralClockOn GPIOA
  GPIO.pinMode (GPIOA,Pin_1) InputAnalog

  let samples :: Num x => x
      samples = 1000
      bufferSize :: Num x => x
      bufferSize = samples *2
  let dmaBuffer = 0x20001000
      dmaConfig = DMA.Config {
        _BufferSize         = samples
       ,_Direction          = PeripheralSRC
       ,_MemoryBaseAddr     = dmaBuffer
       ,_MemoryDataSize     = HalfWord
       ,_MemoryInc          = True
       ,DMA._Mode           = Circular
       ,_PeripheralBaseAddr = regToAddr ADC1 DR
       ,_PeripheralDataSize = HalfWord
       ,_PeripheralInc      = False
       ,_Priority           = High
      }

  peripheralClockOn DMA1
  DMA.deInit DMA1_Channel1
  DMA.init DMA1_Channel1 dmaConfig
  DMA.enable DMA1_Channel1

  let adcConfig = ADC.Config {
     ADC._Mode           =  Independent
    ,_ScanConvMode       = True
    ,_ContinuousConvMode = True
    ,_ExternalTrigConv   = ExternalTrigConv_None
    ,_DataAlign          = AlignRight
    ,_NbrOfChannel       = 1
    }
  peripheralClockOn ADC1
  ADC.init ADC1 adcConfig

  ADC.regularChannelConfig ADC1 Channel_1 1 SampleTime_239Cycles5
--  ADC.regularChannelConfig ADC1 Channel_3 2 SampleTime_71Cycles5
--  ADC.regularChannelConfig ADC1 Channel_5 3 SampleTime_71Cycles5

  ADC.dmaCmd ADC1 True
  ADC.cmd ADC1 True
  -- todo : implement calibration
  ADC.softwareStartConvCmd ADC1 True

  liftIO $ putStrLn "sampling"
  delay 1000000
  liftIO $ putStrLn "sampling OK"
  buffer <- readMem8 dmaBuffer bufferSize
  let
    vals :: [(Int,Word16)]
    vals = zip [0..] $ runGet (replicateM samples getWord16le)
              $ BSL.fromStrict buffer
    out = concatMap (\(idx,val) -> show idx ++"," ++ show val ++ "\n") vals
  liftIO $ writeFile filename out

