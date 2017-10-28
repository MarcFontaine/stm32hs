{-# LANGUAGE RankNTypes #-}
----------------------------------------------------------------------------
-- |
-- Module      :  STM32.STLinkUSB
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module exports a small driver for the STLink dongles. 
-- The focus of this API is on reading and writing
-- to the memory of and attached STM32 controller.
-- The STM32 architecture use memory mapped IO registers to program
-- IO ports a hardware peripherals.
-- Therefor a STLink dongle with an attached STM32 board in combination with this
-- library makes a nice Haskell-controlled IO extension board.


module STM32.STLinkUSB
(
   test
  ,STLT
  ,STL
  ,STLinkEnv
  ,runSTLink
  ,initDongle
  ,resetHalt
  ,writeDebugReg
  ,writeMem8
  ,writeMem32
  ,readMem8
  ,readMem32
  ,LogLevel
  ,Logger
  ,xfer
  )
where
import STM32.STLinkUSB.Env
import STM32.STLinkUSB.USBXfer
import STM32.STLinkUSB.MemRW
import STM32.STLinkUSB.Dongle
import STM32.STLinkUSB.Test
import STM32.STLinkUSB.CortexM
