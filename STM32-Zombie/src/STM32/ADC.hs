----------------------------------------------------------------------------
-- |
-- Module      :  STM32.APP
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Analog Digital Converter
{-# LANGUAGE OverloadedStrings #-}
module STM32.ADC
where

import Device
import STM32.MachineInterface
import STM32.Utils
import Data.Word
import qualified STM32.RCC as RCC
import Data.Bits

deInit :: Peripheral -> MI ()
deInit = RCC.peripheralResetToggle

data Config = Config
  {
   _Mode               :: Mode
  ,_ScanConvMode       :: Bool
  ,_ContinuousConvMode :: Bool
  ,_ExternalTrigConv   :: ExternalTrigConv
  ,_DataAlign          :: DataAlign
  ,_NbrOfChannel       :: Word32
  } deriving Show

data Mode
  = Independent
  | RegInjecSimult
  | RegSimult_AlterTrig
  | InjecSimult_FastInterl
  | InjecSimult_SlowInterl
  | InjecSimult
  | RegSimult
  | FastInterl
  | SlowInterl
  | AlterTrig
  deriving (Show)

instance RegisterField Mode where
  toBits m = case m of
    Independent             -> "0000"
    RegInjecSimult          -> "0001"
    RegSimult_AlterTrig     -> "0010"
    InjecSimult_FastInterl  -> "0011"
    InjecSimult_SlowInterl  -> "0100"
    InjecSimult             -> "0101"
    RegSimult               -> "0110"
    FastInterl              -> "0111"
    SlowInterl              -> "1000"
    AlterTrig               -> "1001"
  toField = const CR1_DUALMOD

data ExternalTrigConv
  = ExternalTrigConv_T1_CC1
  | ExternalTrigConv_T1_CC2
  | ExternalTrigConv_T1_CC3
  | ExternalTrigConv_T2_CC2
  | ExternalTrigConv_T3_TRGO
  | ExternalTrigConv_T4_CC4
  | ExternalTrigConv_Ext_IT11_TIM8_TRGO
  | ExternalTrigConv_None
  | ExternalTrigConv_T3_CC1
  | ExternalTrigConv_T2_CC3
  | ExternalTrigConv_T8_CC1
  | ExternalTrigConv_T8_TRGO
  | ExternalTrigConv_T5_CC1
  | ExternalTrigConv_T5_CC3
  deriving Show

instance RegisterField ExternalTrigConv where
  toBits x = case x of
    ExternalTrigConv_T1_CC1                -> "000"
    ExternalTrigConv_T1_CC2                -> "001"
    ExternalTrigConv_T1_CC3                -> "010"
    ExternalTrigConv_T2_CC2                -> "011"
    ExternalTrigConv_T3_TRGO               -> "100"
    ExternalTrigConv_T4_CC4                -> "101"
    ExternalTrigConv_Ext_IT11_TIM8_TRGO    -> "110"
    ExternalTrigConv_None                  -> "111"
    ExternalTrigConv_T3_CC1                -> "000"
    ExternalTrigConv_T2_CC3                -> "001"
    ExternalTrigConv_T8_CC1                -> "011"
    ExternalTrigConv_T8_TRGO               -> "100"
    ExternalTrigConv_T5_CC1                -> "101"
    ExternalTrigConv_T5_CC3                -> "110"
  toField = const CR2_EXTSEL

data DataAlign = AlignRight | AlignLeft
  deriving Show

instance ToBit DataAlign where
  toBit AlignRight = False
  toBit AlignLeft  = True


data Channel
  = Channel_0 | Channel_1 | Channel_2 | Channel_3 | Channel_4 | Channel_5
  | Channel_6 | Channel_7 | Channel_8 | Channel_9 | Channel_10 | Channel_11
  | Channel_12 | Channel_13 | Channel_14 | Channel_15 | Channel_16 | Channel_17
  deriving Show

data SampleTime
  = SampleTime_1Cycles5
  | SampleTime_7Cycles5
  | SampleTime_13Cycles5
  | SampleTime_28Cycles5
  | SampleTime_41Cycles5
  | SampleTime_55Cycles5
  | SampleTime_71Cycles5
  | SampleTime_239Cycles5
  deriving Show

instance ToBitField SampleTime where
  toBitField s = case s of
    SampleTime_1Cycles5   -> "000"
    SampleTime_7Cycles5   -> "001"
    SampleTime_13Cycles5  -> "010"
    SampleTime_28Cycles5  -> "011"
    SampleTime_41Cycles5  -> "100"
    SampleTime_55Cycles5  -> "101"
    SampleTime_71Cycles5  -> "110"
    SampleTime_239Cycles5 -> "111"

data ExternalTrigInjecConv
  = ExternalTrigInjecConv_T1_TRGO
  | ExternalTrigInjecConv_T1_CC4
  | ExternalTrigInjecConv_T2_TRGO
  | ExternalTrigInjecConv_T2_CC1
  | ExternalTrigInjecConv_T3_CC4
  | ExternalTrigInjecConv_T4_TRGO
  | ExternalTrigInjecConv_Ext_IT15_TIM8_CC4
  | ExternalTrigInjecConv_None
  | ExternalTrigInjecConv_T4_CC3
  | ExternalTrigInjecConv_T8_CC2
  | ExternalTrigInjecConv_T8_CC4
  | ExternalTrigInjecConv_T5_TRGO
  | ExternalTrigInjecConv_T5_CC4
  deriving Show

instance ToBitField ExternalTrigInjecConv where
  toBitField e = case e of
    ExternalTrigInjecConv_T1_TRGO           -> "000"
    ExternalTrigInjecConv_T1_CC4            -> "001"
    ExternalTrigInjecConv_T2_TRGO           -> "010"
    ExternalTrigInjecConv_T2_CC1            -> "011"
    ExternalTrigInjecConv_T3_CC4            -> "100"
    ExternalTrigInjecConv_T4_TRGO           -> "101"
    ExternalTrigInjecConv_Ext_IT15_TIM8_CC4 -> "110"
    ExternalTrigInjecConv_None              -> "111"
    ExternalTrigInjecConv_T4_CC3            -> "010"
    ExternalTrigInjecConv_T8_CC2            -> "011"
    ExternalTrigInjecConv_T8_CC4            -> "100"
    ExternalTrigInjecConv_T5_TRGO           -> "101"
    ExternalTrigInjecConv_T5_CC4            -> "110"

data InjectedChannel
  = InjectedChannel_1
  | InjectedChannel_2
  | InjectedChannel_3
  | InjectedChannel_4
  deriving Show

data AnalogWatchdog
  = AnalogWatchdog_SingleRegEnable
  | AnalogWatchdog_SingleInjecEnable
  | AnalogWatchdog_SingleRegOrInjecEnable
  | AnalogWatchdog_AllRegEnable
  | AnalogWatchdog_AllInjecEnable
  | AnalogWatchdog_AllRegAllInjecEnable
  | AnalogWatchdog_None
  deriving Show

init :: Peripheral -> Config -> MI ()
init p conf = do
  fieldWrite p $ _Mode conf
  bitWrite p CR1_SCAN $ _ScanConvMode conf

  bitWrite p CR2_ALIGN $ _DataAlign conf
  fieldWrite p  $ _ExternalTrigConv conf
  bitWrite p CR2_CONT $ _ContinuousConvMode conf

  pokeReg p SQR1 ((_NbrOfChannel conf -1) `shiftL` 20)

channelToSMP :: Channel -> Field
channelToSMP ch = case ch of
  Channel_0 -> SMPR2_SMP0
  Channel_1 -> SMPR2_SMP1
  Channel_2 -> SMPR2_SMP2
  Channel_3 -> SMPR2_SMP3
  Channel_4 -> SMPR2_SMP4
  Channel_5 -> SMPR2_SMP5
  Channel_6 -> SMPR2_SMP6
  Channel_7 -> SMPR2_SMP7
  Channel_8 -> SMPR2_SMP8
  Channel_9 -> SMPR2_SMP9
  Channel_10 -> SMPR1_SMP10
  Channel_11 -> SMPR1_SMP11
  Channel_12 -> SMPR1_SMP12
  Channel_13 -> SMPR1_SMP13
  Channel_14 -> SMPR1_SMP14
  Channel_15 -> SMPR1_SMP15
  Channel_16 -> SMPR1_SMP16
  Channel_17 -> SMPR1_SMP17

channelToSQBits :: Channel -> BitField
channelToSQBits ch = case ch of
  Channel_0 -> "00000"
  Channel_1 -> "00001"
  Channel_2 -> "00010"
  Channel_3 -> "00011"
  Channel_4 -> "00100"
  Channel_5 -> "00101"
  Channel_6 -> "00110"
  Channel_7 -> "00111"
  Channel_8 -> "01000"
  Channel_9 -> "01001"
  Channel_10 -> "01010"
  Channel_11 -> "01011"
  Channel_12 -> "01100"
  Channel_13 -> "01101"
  Channel_14 -> "01110"
  Channel_15 -> "01111"
  Channel_16 -> "10000"
  Channel_17 -> "10001"

rankToSQ :: Word8 -> Field
rankToSQ r = case r of
  1 -> SQR3_SQ1
  2 -> SQR3_SQ2
  3 -> SQR3_SQ3
  4 -> SQR3_SQ4
  5 -> SQR3_SQ5
  6 -> SQR3_SQ6
  7 -> SQR2_SQ7
  8 -> SQR2_SQ8
  9 -> SQR2_SQ9
  10 -> SQR2_SQ10
  11 -> SQR2_SQ11
  12 -> SQR2_SQ12
  13 -> SQR1_SQ13
  14 -> SQR1_SQ14
  15 -> SQR1_SQ15
  16 -> SQR1_SQ16
  _ -> error "ADC.hs rankToSQ"

regularChannelConfig :: Peripheral -> Channel -> Word8 -> SampleTime -> MI ()
regularChannelConfig p channel rank sampleTime = do
  regFieldWrite p (channelToSMP channel) sampleTime
  regFieldWrite p (rankToSQ rank) (channelToSQBits channel)

dmaCmd :: Peripheral -> Bool -> MI ()
dmaCmd p rs = case p of
  ADC1 -> bitWrite ADC1 CR2_DMA rs
  ADC2 -> error "dmaCMD: ADC2 no DMA available"
  ADC3 -> bitWrite ADC3 CR2_DMA rs
  _    -> error "dmaCMD"

cmd :: Peripheral -> Bool -> MI ()
cmd p rs = bitWrite p CR2_ADON rs

softwareStartConvCmd :: Peripheral -> Bool -> MI ()
softwareStartConvCmd p rs = do
  bitWrite p CR2_EXTTRIG rs
  bitWrite p CR2_SWSTART rs
