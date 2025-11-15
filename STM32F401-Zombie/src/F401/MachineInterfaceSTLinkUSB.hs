----------------------------------------------------------------------------
-- |
-- Module      :  F401.MachineInterfaceSTLinkUSB
-- Copyright   :  (c) Marc Fontaine 2025
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- F401.MachineInterfaceSTLinkUSB is the (internal)
-- API for communication with the F401Fxxx boards
-- All communication runs through these function.
-- The main driver for ST-Link USB dongles is in the STLinkUSB package.
-- This module contains some small wrappers for functions from F401.STLinkUSB.
--
{-# LANGUAGE RankNTypes #-}

module F401.MachineInterfaceSTLinkUSB
(
   MI
  ,runMI
  ,initMI
  ,F401.MachineInterfaceSTLinkUSB.resetHalt
  ,peek_w16 -- check if supported by hardware if not remove
  ,poke_w16 -- if i remember right hardware implements poke_w16 as 2 poke_w8
            -- that is very bad if used on the bitbang region
  ,peek_w32
  ,poke_w32

   -- Unused at the moment :
  ,F401.MachineInterfaceSTLinkUSB.writeMem8
  ,F401.MachineInterfaceSTLinkUSB.writeMem32
  ,F401.MachineInterfaceSTLinkUSB.readMem8
  ,F401.MachineInterfaceSTLinkUSB.readMem32
)

where
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL (fromStrict, toStrict)
import           Data.Word

import           Data.Binary.Get
import           Data.Binary.Put

import           STM32.STLinkUSB
import           STM32.STLinkUSB.Env
import           STM32.STLinkUSB.LibUSB     (findEndpoint, withEndpoint)

type Addr = Word32
--type MI a = STLT IO a
type MI a = forall m. MonadIO m => STLT m a

runMI :: MI a -> IO a
runMI = runSTLink' defaultDebugLogger findEndpoint withEndpoint . runReaderT

initMI :: MonadIO m => STLT m ()
initMI = initDongle

resetHalt :: MonadIO m => STLT m ()
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
