-- |
-- Module      :  STM32.STLinkUSB.CortexM
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Starting and stopping the attached CPU

module STM32.STLinkUSB.CortexM
where

import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import Control.Monad

import STM32.STLinkUSB.Commands
import STM32.STLinkUSB.Dongle
import STM32.STLinkUSB.Env
import STM32.STLinkUSB.USBXfer
import STM32.STLinkUSB.MemRW
  
halt :: STL ()
halt = do
  debugSTL Info "halting CPU"
  api <- asksDongleAPI              
  case api of
     APIV2 -> writeDebugReg _DCB_DHCSR (_DBGKEY .|. _C_HALT .|. _C_DEBUGEN)
     APIV1 -> void $ xfer (DEBUG_COMMAND FORCEDEBUG)

{-- TODO: does this work ?
-- The intended application is to use the micro controller as an IO extension
-- board and write to the hardware register over the SWD interface.
-- It is essential that the micro controller cpu does not interfere,
-- i.e. it is halted.
-- (fancy stuff could be possible by running a custom micro controller firmware)
-}
resetHalt :: STL ()
resetHalt = halt >> reset

run :: STL ()
run = do
  debugSTL Info "starting CPU"
  api <- asksDongleAPI              
  case api of
     APIV2 -> writeDebugReg _DCB_DHCSR (_DBGKEY .|. _C_DEBUGEN)
     APIV1 -> void $ xfer (DEBUG_COMMAND RUNCORE)

readCpuID :: STL BS.ByteString
readCpuID = do
  debugSTL Info ("trying to read CPU ID")
  cpuID <- readMem32 _CPUID 4
  debugSTL Info ("CPU ID : " ++ (show $ BS.unpack cpuID))
  return cpuID

_CPUID :: Word32
_CPUID = 0xE000ED00

_DCB_DHCSR :: Word32
_DCB_DCRSR :: Word32
_DCB_DCRDR :: Word32
_DCB_DEMCR :: Word32

_DCB_DHCSR = 0xE000EDF0
_DCB_DCRSR = 0xE000EDF4
_DCB_DCRDR = 0xE000EDF8
_DCB_DEMCR = 0xE000EDFC

_DBGKEY :: Word32
_DBGKEY = 0xA05F0000

_C_DEBUGEN :: Word32
_C_DEBUGEN = 1
_C_HALT :: Word32
_C_HALT = 2
