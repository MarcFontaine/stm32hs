----------------------------------------------------------------------------
-- |
-- Module      :  STM32.STLinkUSB.Test
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Test the connetion to the STLink dongle
-- and read the CPU ID of the attached controller.

{-# LANGUAGE RankNTypes #-}
module STM32.STLinkUSB.Test
where

import STM32.STLinkUSB.Env
import STM32.STLinkUSB.Dongle
import STM32.STLinkUSB.CortexM

-- | Test the dongle and connection to the board.
-- This test fails if no board is attached
test :: IO ()
test = runSTLink_verbose $ do
  initDongle
  _<-readCpuID
  return ()
