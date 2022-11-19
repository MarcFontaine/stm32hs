----------------------------------------------------------------------------
-- |
-- Module      :  STM32.MachineInterfaceSTLinkUSB
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- STM32.MachineInterfaceSTLinkUSB is the (internal)
-- API for communication with the STM32Fxxx boards
-- All communication runs through these function.
-- The main driver for ST-Link USB dongles is in the STLinkUSB package.
-- This module contains some small wrappers for functions from STM32.STLinkUSB.
--

module STM32.MachineInterfaceSTLinkUSB
(
   MI
  ,runMI
  ,initMI
  ,STM32.MachineInterfaceSTLinkUSB.resetHalt
  ,peek_w16 -- check if supported by hardware if not remove
  ,poke_w16 -- if i remember right hardware implements poke_w16 as 2 poke_w8
            -- that is very bad if used on the bitbang region
  ,peek_w32
  ,poke_w32

   -- Unused at the moment :
  ,STM32.MachineInterfaceSTLinkUSB.writeMem8
  ,STM32.MachineInterfaceSTLinkUSB.writeMem32
  ,STM32.MachineInterfaceSTLinkUSB.readMem8
  ,STM32.MachineInterfaceSTLinkUSB.readMem32
)

where

import STM32.STLinkUSB

import Data.Word
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL (toStrict,fromStrict)

import Data.Binary.Put
import Data.Binary.Get

type Addr = Word32
type MI a = STLT IO a

runMI :: MI a -> IO a
runMI = runSTLink

initMI :: MI ()
initMI = initDongle

resetHalt :: MI ()
resetHalt = STM32.STLinkUSB.resetHalt

peek_w16 :: Addr -> MI Word16
peek_w16 addr = do
  bs <- STM32.STLinkUSB.readMem8 addr 2
  return $ runGet getWord16le $ BSL.fromStrict bs

peek_w32 :: Addr -> MI Word32
peek_w32 addr = do
  bs <- STM32.STLinkUSB.readMem32 addr 4
  return $ runGet getWord32le $ BSL.fromStrict bs

poke_w16 :: Addr -> Word16 -> MI ()
poke_w16 addr val
  = STM32.STLinkUSB.writeMem8 addr $ BSL.toStrict $ runPut $ putWord16le val

poke_w32 :: Addr -> Word32 -> MI ()
poke_w32 addr val
  = STM32.STLinkUSB.writeMem32 addr $ BSL.toStrict $ runPut $ putWord32le val

writeMem8 :: Addr -> BS.ByteString -> MI ()
writeMem8 = STM32.STLinkUSB.writeMem8

writeMem32 :: Addr -> BS.ByteString -> MI ()
writeMem32 = STM32.STLinkUSB.writeMem32

readMem8 :: Addr -> Int -> MI BS.ByteString
readMem8 = STM32.STLinkUSB.readMem8

readMem32 :: Addr -> Int -> MI BS.ByteString
readMem32 = STM32.STLinkUSB.readMem32
