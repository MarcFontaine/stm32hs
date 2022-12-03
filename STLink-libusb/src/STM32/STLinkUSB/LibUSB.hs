----------------------------------------------------------------------------
-- |
-- Module      :  STM32.STLinkUSB.LibUSB
-- Copyright   :  (c) Marc Fontaine 2017-2022
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- This module contains low-level functions for initializing the USB connection.
-- In most setups 'STM32.STLinkUSB.Env.runSTLink' does all the work
-- and there is no need to include this module.

{-# LANGUAGE LambdaCase #-}

module STM32.STLinkUSB.LibUSB
  (
    findEndpoint
  , withEndpoint
  )
where

import Control.Exception (SomeException(..), catch)
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.ByteString as BS
import qualified Data.Vector as Vector

import System.USB hiding (Status(..), USBException)
import qualified System.USB as USB

import STM32.STLinkUSB.USB

findEndpoint
  :: IO (Ctx, Device, EndpointAddress, EndpointAddress, EndpointAddress)
findEndpoint = do
  ctx <- newCtx
  setDebug ctx PrintWarnings
  list <- findUSBDevices ctx defaultSTLProductID
  let device = case list of
        [x] -> x
        [] -> error "no STLink dongle found"
        (_:_:_) -> error "more than one STLink dongle found"

  findEndpoints ctx device
  where
    findEndpoints
       :: Ctx
       -> Device
       -> IO (Ctx, Device, EndpointAddress, EndpointAddress, EndpointAddress)
    findEndpoints ctx device = do
      config <- getConfigDesc device 0
      let
         endPoints = interfaceEndpoints
                       $ Vector.head $ Vector.head $ configInterfaces config
         rxEndpoint = endpointAddress $ endPoints Vector.! 0
         txEndpoint = endpointAddress $ endPoints Vector.! 1
         traceEndpoint = endpointAddress $ endPoints Vector.! 2
      return (ctx, device, rxEndpoint, txEndpoint, traceEndpoint)

findUSBDevices :: Ctx -> ProductId -> IO [Device]
findUSBDevices ctx stlProductID = do
  devices <- Vector.toList <$> getDevices ctx
  flip filterM devices $ \ device -> do
      descr <- getDeviceDesc device
      return (deviceProductId descr == stlProductID)

defaultSTLProductID :: ProductId
defaultSTLProductID = 0x3748

-- type WithEndpoint a b = a -> ((ReadBulk, WriteBulk) -> IO b) -> IO b

withEndpoint 
  :: (Ctx, Device, EndpointAddress, EndpointAddress, EndpointAddress)
  -> ((ReadBulk, WriteBulk) -> IO b)
  -> IO b
withEndpoint  (_ctx, device, rxEndpoint, txEndpoint, _traceEndpoint) action
  = withDeviceHandle device $
     \deviceHandle  -> withDetachedKernelDriver deviceHandle 0
       $ action ( readBulkCallback deviceHandle rxEndpoint
                , writeBulkCallback deviceHandle txEndpoint)

readBulkCallback :: DeviceHandle -> EndpointAddress -> ReadBulk
readBulkCallback deviceHandle rxEndpoint = liftIO $ catch readAction handler
  where
    readAction = do
      (r, s) <- USB.readBulk deviceHandle rxEndpoint 64 usbReadTimeout
      return (r, Right $ fromUSBStatus s)

    handler :: USB.USBException -> IO (BS.ByteString, Either USBException Status)
    handler e = return  (BS.empty, Left $ SomeException e)

writeBulkCallback :: DeviceHandle -> EndpointAddress -> WriteBulk
writeBulkCallback deviceHandle txEndpoint byteString = do
  (size, status) <- USB.writeBulk deviceHandle txEndpoint byteString usbWriteTimeout
  return (size, fromUSBStatus status)

usbReadTimeout :: Int
usbReadTimeout = 1000

usbWriteTimeout :: Int
usbWriteTimeout = 1000

fromUSBStatus :: USB.Status -> Status
fromUSBStatus = \case
  USB.Completed -> Completed
  USB.TimedOut  -> TimedOut

