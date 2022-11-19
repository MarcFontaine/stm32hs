----------------------------------------------------------------------------
-- |
-- Module      :  STM32.Timer
-- Copyright   :  (c) Marc Fontaine 2017-2022
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Timer

{-# LANGUAGE OverloadedStrings,NoMonomorphismRestriction #-}
module STM32.Timer
where

import Device
import STM32.MachineInterface
import STM32.Utils
import qualified STM32.RCC as RCC

import Data.Word

data TimeBase = TimeBase {
   _Prescaler          :: Word16
  ,_CounterMode        :: CounterMode
  ,_Period             :: Word16
  ,_ClockDevision      :: ClockDevision
  ,_RepetitionCounter  :: Word8
  }  deriving Show

data CounterMode
  = Up
  | Down
  | CenterAligned1
  | CenterAligned2
  | CenterAligned3
  deriving Show

data ClockDevision = CKD_DIV1 | CKD_DIV2 | CKD_DIV4
  deriving Show

instance RegisterField ClockDevision where
  toBits b = case b of
     CKD_DIV1 -> "00"
     CKD_DIV2 -> "01"
     CKD_DIV4 -> "10"
  toField = const CR1_CKD

deInit :: Peripheral -> MI ()
deInit = RCC.peripheralResetToggle

timeBaseInit :: Peripheral -> TimeBase -> MI ()
timeBaseInit p conf = do
  fieldWrite p $ _ClockDevision conf
  let
    mode :: BitField
    mode = case _CounterMode conf of
      Up             -> "00"
      Down           -> "00"
      CenterAligned1 -> "01"
      CenterAligned2 -> "10"
      CenterAligned3 -> "11"
  regFieldWrite p CR1_CMS mode

  bitWrite p CR1_DIR $ case _CounterMode conf of
    Up             -> False
    Down           -> True
    _              -> True
  pokeReg p ARR $ fromIntegral $ _Period conf
  pokeReg p PSC $ fromIntegral $ _Prescaler conf
  bitSet p EGR_UG -- generate and update-event
  if ( p==TIM1 || p== TIM8)
     then pokeReg p RCR $ fromIntegral $ _RepetitionCounter conf
     else return ()
