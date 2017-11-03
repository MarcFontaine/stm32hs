----------------------------------------------------------------------------
-- |
-- Module      :  STM32.DAC
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Digital Analog Converters.
-- This is untested.
-- The cheap STM32F103C8T6 boards don't have a built-in DAC.
{-# LANGUAGE OverloadedStrings #-}
module STM32.DAC
where

import Data.Word
import Data.Bits
import Device
import STM32.MachineInterface
import STM32.Utils

import qualified STM32.RCC as RCC (peripheralResetToggle)
       
data Config = Config {
    _trigger :: Maybe Trigger
   ,_waveGeneration :: Maybe Wave
   ,_LFSRUnmask_TriangleAmplitude :: Either TriangleAmplitude LFSRUnmask
   ,_outputBuffer :: Bool
   } deriving (Show,Eq)

defaultConfig :: Config
defaultConfig = Config {
    _trigger = Nothing
   ,_waveGeneration = Nothing
   ,_LFSRUnmask_TriangleAmplitude = Right Bit0
   ,_outputBuffer = True
   }              
   
data Trigger
  = T6_TRGO | T8_TRGO | T7_TRGO
  | T5_TRGO | T2_TRGO | T4_TRGO | Ext_IT9 | Software
  deriving (Show,Eq)

instance ToBitField Trigger where
  toBitField t = case t of
    T6_TRGO  -> "000"
    T8_TRGO  -> "001"
    T7_TRGO  -> "010"
    T5_TRGO  -> "011"
    T2_TRGO  -> "100"
    T4_TRGO  -> "101"
    Ext_IT9  -> "110"
    Software -> "111"
  
data Wave = Noise | Triangle
  deriving (Show,Eq)

data TriangleAmplitude
  = Amplitude_1 | Amplitude_3 | Amplitude_7 | Amplitude_15
  | Amplitude_31 | Amplitude_63
  | Amplitude_127 | Amplitude_255 | Amplitude_511 | Amplitude_1023
  | Amplitude_2047 | Amplitude_4095
  deriving (Show,Eq)

instance ToBitField TriangleAmplitude where
  toBitField t = case t of
    Amplitude_1      -> "0000"
    Amplitude_3      -> "0001"
    Amplitude_7      -> "0010" 
    Amplitude_15     -> "0011"
    Amplitude_31     -> "0100"
    Amplitude_63     -> "0101"
    Amplitude_127    -> "0110"
    Amplitude_255    -> "0111"
    Amplitude_511    -> "1000"
    Amplitude_1023   -> "1001"
    Amplitude_2047   -> "1010"
    Amplitude_4095   -> "1011"
  
data LFSRUnmask
  = Bit0 | Bits1 | Bits2 | Bits3 | Bits4 | Bits5 | Bits6 | Bits7
  | Bits8 | Bits9 | Bits10 | Bits11
  deriving (Show,Eq)

instance ToBitField LFSRUnmask where
  toBitField t = case t of
    Bit0   -> "0000"
    Bits1  -> "0001"
    Bits2  -> "0010"
    Bits3  -> "0011"
    Bits4  -> "0100"
    Bits5  -> "0101"
    Bits6  -> "0110"
    Bits7  -> "0111"
    Bits8  -> "1000"
    Bits9  -> "1001"
    Bits10  -> "1010"
    Bits11  -> "1011"
  
data Channel = Channel_1 | Channel_2 deriving (Show,Eq)

data Align = Align_12b_R | Align_12b_L | Align_8b_R  deriving (Show,Eq)
     
deInit :: MI ()
deInit = RCC.peripheralResetToggle DAC

init :: Channel ->  Config -> MI ()
init channel config = do
  let (tsel,wave,mamp,boff) = case channel of
        Channel_1 -> (CR_TSEL1,CR_WAVE1,CR_MAMP1,CR_BOFF1)
        Channel_2 -> (CR_TSEL2,CR_WAVE2,CR_MAMP2,CR_BOFF2)

  regFieldWrite DAC tsel $ case _trigger config of
      Nothing -> "000"
      Just t  -> toBitField t

  regFieldWrite DAC wave $ case _waveGeneration config of
      Nothing    -> BitField [False,False]
      Just Noise -> BitField [False,True]
      Just Triangle -> BitField [True,False]

  regFieldWrite DAC mamp $ case _LFSRUnmask_TriangleAmplitude config of
      Right t -> toBitField t
      Left  t -> toBitField t

  bitWrite DAC boff $ not $ _outputBuffer config

cmd :: Channel -> Bool -> MI ()
cmd Channel_1 rs = bitWrite DAC CR_EN1 rs
cmd Channel_2 rs = bitWrite DAC CR_EN2 rs
    
enable :: Channel -> MI ()
enable c = cmd c True
       
disable :: Channel -> MI ()
disable c = cmd c False

dmaCmd :: Channel -> Bool -> MI ()        
dmaCmd Channel_1 rs = bitWrite DAC CR_DMAEN1 rs
dmaCmd Channel_2 rs = bitWrite DAC CR_DMAEN2 rs

        
enableDMA :: Channel -> MI ()
enableDMA c = dmaCmd c True
          
disableDMA :: Channel -> MI ()
disableDMA c = dmaCmd c False

softwareTriggerCmd  :: Channel -> Bool -> MI ()
softwareTriggerCmd Channel_1 rs = bitWrite DAC SWTRIGR_SWTRIG1 rs
softwareTriggerCmd Channel_2 rs = bitWrite DAC SWTRIGR_SWTRIG2 rs
                    
enableSoftwareTrigger :: Channel -> MI ()
enableSoftwareTrigger c = softwareTriggerCmd c True
                      
disableSoftwareTrigger :: Channel -> MI ()
disableSoftwareTrigger c = softwareTriggerCmd c False

dualSoftwareTriggerCmd :: Bool -> MI ()
dualSoftwareTriggerCmd rs = do
  softwareTriggerCmd Channel_1 rs
  softwareTriggerCmd Channel_2 rs
                       
enableDualSoftwareTrigger :: MI ()
enableDualSoftwareTrigger = dualSoftwareTriggerCmd True
                          
disableDualSoftwareTrigger :: MI ()
disableDualSoftwareTrigger = dualSoftwareTriggerCmd False

waveGenerationCmd :: Channel -> (Maybe Wave) -> MI ()
waveGenerationCmd ch wave = regFieldWrite DAC register bits
  where
     register = case ch of
       Channel_1 -> CR_WAVE1
       Channel_2 -> CR_WAVE2
     bits :: BitField
     bits = case wave of
       Nothing        -> "00"
       (Just Noise)   -> "01"
       (Just Triangle)-> "10"
                        
disableWaveGeneration :: Channel -> MI ()
disableWaveGeneration ch = waveGenerationCmd ch Nothing
                         
setChannel1Data :: Align -> Word16 -> MI ()
setChannel1Data align w = pokeReg DAC reg $ fromIntegral w
   where 
     reg = case align of
       Align_12b_R -> DHR12R1
       Align_12b_L -> DHR12L1
       Align_8b_R  -> DHR8R1

setChannel2Data :: Align -> Word16 -> MI ()
setChannel2Data align w = pokeReg DAC reg $ fromIntegral w
   where 
     reg = case align of
       Align_12b_R -> DHR12R2
       Align_12b_L -> DHR12L2
       Align_8b_R  -> DHR8R2

setDualChannelData :: Align -> Word16 -> Word16 -> MI ()
setDualChannelData align w1 w2
  = pokeReg DAC reg $ (fromIntegral w1) .|. (byteSwap32 $ fromIntegral w2)
  where 
     reg = case align of
       Align_12b_R -> DHR12RD
       Align_12b_L -> DHR12LD
       Align_8b_R  -> DHR8RD

getDataOutputValue :: Channel -> MI (Word16)
getDataOutputValue Channel_1 = fmap fromIntegral $ peekReg DAC DOR1
getDataOutputValue Channel_2 = fmap fromIntegral $ peekReg DAC DOR2 

setChannel1 :: Word16 -> MI ()
setChannel1 = setChannel1Data Align_12b_R

setChannel2 :: Word16 -> MI ()
setChannel2 = setChannel2Data Align_12b_R

setDualChannel :: Word16 -> Word16 -> MI ()
setDualChannel = setDualChannelData Align_12b_R               
