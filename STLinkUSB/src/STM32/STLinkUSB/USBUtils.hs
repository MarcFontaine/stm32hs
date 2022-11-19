----------------------------------------------------------------------------
-- |
-- Module      :  STM32.STLinkUSB.USBUtils
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

module STM32.STLinkUSB.USBUtils
where
import qualified Data.Vector as Vector
import System.USB
import Control.Monad

findDefaultEndpoints
  :: IO (Ctx, Device, EndpointAddress, EndpointAddress, EndpointAddress)
findDefaultEndpoints = do
  ctx <- newCtx
  setDebug ctx PrintWarnings
  list <- findUSBDevices ctx defaultSTLProductID
  let device = case list of
        [x] -> x
        [] -> error "no STLink dongle found"
        (_:_:_) -> error "more than one STLink dongle found"

  findEndpoints ctx device

findUSBDevices ::
  Ctx -> ProductId -> IO [Device]
findUSBDevices ctx stlProductID = do
  devices <- Vector.toList <$> getDevices ctx
  flip filterM devices $ \ device -> do
      descr <- getDeviceDesc device
      return (deviceProductId descr == stlProductID)

defaultSTLProductID :: ProductId
defaultSTLProductID = 0x3748

findEndpoints ::
     Ctx -> Device
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

withUSB :: Device -> (DeviceHandle -> IO a) -> IO a
withUSB device action
  = withDeviceHandle device $
     \deviceHandle  -> withDetachedKernelDriver deviceHandle 0
                         $ action deviceHandle
