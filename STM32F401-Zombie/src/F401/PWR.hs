----------------------------------------------------------------------------
-- |
-- Module      :  F401.PWR
-- Copyright   :  (c) Marc Fontaine 2025
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only

{-# LANGUAGE OverloadedStrings #-}
module F401.PWR
where

import Device
import F401.MachineInterface
import F401.Utils
import qualified F401.RCC as RCC

data PVDLevel = U_2V2 | U_2V3 | U_2V4 | U_2V5 | U_2V6 | U_2V7 | U_2V8 | U_2V9
  deriving (Show,Eq)

instance RegisterField PVDLevel where
  toBits b = case b of
    U_2V2 -> "000"
    U_2V3 -> "001"
    U_2V4 -> "010"
    U_2V5 -> "011"
    U_2V6 -> "100"
    U_2V7 -> "101"
    U_2V8 -> "110"
    U_2V9 -> "111"
  toField = const CR_PLS

data Flag = WU | SB | PVDO
  deriving (Show,Eq)

deInit :: MI ()
deInit = RCC.peripheralResetToggle PWR

backupAccessCmd :: Bool -> MI ()
backupAccessCmd = bitWrite PWR CR_DBP

pvdCmd :: Bool -> MI ()
pvdCmd = bitWrite PWR CR_PVDE

pvdLevelConfig :: PVDLevel -> MI ()
pvdLevelConfig = fieldWrite PWR

wakeUpPinCmd :: Bool -> MI ()
wakeUpPinCmd = bitWrite PWR CSR_EWUP

getFlagStatus :: Flag -> MI Bool
getFlagStatus = error "todo"


clearFlag :: Flag -> MI ()
clearFlag flag = bitSet PWR $ case flag of
  WU   -> CR_CWUF
  SB   -> CR_CSBF
  PVDO -> CR_PVDE
