----------------------------------------------------------------------------
-- |
-- Module      :  App.RealTimeClock
-- License     :  BSD3
-- 
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Read the real time clock.
-- This only works if the controller has a battery installed
-- and the RTC has been initialized.

module App.RealTimeClock
where
import Control.Monad

import STM32.API
import qualified STM32.RTC as RTC

printRTC :: IO ()       
printRTC = runMI $ do
  initMI
  resetHalt  
  RTC.getCounter >>=print'
