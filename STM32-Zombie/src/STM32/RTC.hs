----------------------------------------------------------------------------
-- |
-- Module      :  STM32.RTC
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The real time clock.

module STM32.RTC
where

import Data.Word
import Data.Bits
import Device
import STM32.MachineInterface
import STM32.Utils
import qualified STM32.PWR as PWR
import qualified STM32.RCC as RCC

getCounter :: MI Word32
getCounter = peekLHReg RTC (CNTL,CNTH)

exitConfigMode :: MI ()
exitConfigMode = bitReset RTC CRL_CNF

enterConfigMode :: MI ()
enterConfigMode = bitSet RTC CRL_CNF

inConfigMode :: MI x -> MI x
inConfigMode action = do
  enterConfigMode
  r <- action
  exitConfigMode
  return r
          
setCounter :: Word32 -> MI ()
setCounter n = inConfigMode $ pokeLHReg RTC (CNTL,CNTH) n

addJustCounter :: Word32 -> MI ()
addJustCounter offset = do
  t <- getCounter
  setCounter $ t + offset


-- | Setup the batterie powered real-time-clock.
-- The board should have a backup battery and a low speed external crystal.
setupLSE_RTC :: Word32 -> MI ()
setupLSE_RTC epoch = do
  RCC.peripheralClockOn BKP
  RCC.peripheralClockOn PWR
  PWR.backupAccessCmd True

  RCC.peripheralResetToggle BKP --   BKP_DeInit(); todo : BKP-module
  RCC.lseConfig RCC.LSE_ON

{-  /* Wait till LSE is ready */
  while (RCC_GetFlagStatus(RCC_FLAG_LSERDY) == RESET)
  {}
-}

  delay 100000
  RCC.rtcClockConfig RCC.LSE
  RCC.rtcClkCmd True

{-
  /* Wait for RTC registers synchronization */
  RTC_WaitForSynchro();

  /* Wait until last write operation on RTC registers has finished */
  RTC_WaitForLastTask();
-}
  delay 100000
  setPrescaler 32767
  delay 100000
  setCounter epoch -- didnt work??

setPrescaler :: Word32 -> MI ()
setPrescaler n
  = inConfigMode $ pokeLHReg RTC (PRLL,PRLH) (n .&. 0x000fffff)
