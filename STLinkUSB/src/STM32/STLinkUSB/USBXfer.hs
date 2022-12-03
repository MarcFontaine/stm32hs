----------------------------------------------------------------------------
-- |
-- Module      :  STM32.STLinkUSB.USBXfer
-- Copyright   :  (c) Marc Fontaine 2017-2022
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
-- This module contains low-level functions for USB data transfers.
-- Don't use theses functions directly, the prefered API is the MemRW module.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module STM32.STLinkUSB.USBXfer
(
    Status (..)
  , xfer
  , xferBulkWrite
  , xferRetry
  , xferReadTrace
  , xferStatus
)
where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)

import qualified Data.ByteString as BS

import STM32.STLinkUSB.Commands hiding (Status)
import STM32.STLinkUSB.Env
import STM32.STLinkUSB.USB

writeCMD :: Cmd -> STL (Size, Status)
writeCMD cmd
  = ReaderT $ \STLinkEnv {..} -> liftIO $ writeBulk $ cmdToByteString cmd

readBulkSTL :: STL (BS.ByteString, Either USBException Status)
readBulkSTL = ReaderT $ \STLinkEnv {..} -> liftIO readBulk

xferStatus :: Cmd -> STL (BS.ByteString, Either USBException Status)
xferStatus cmd = do
  debugSTL Debug $ show ("xferStatus write :", cmd)
  writeResult <- writeCMD cmd
  debugSTL Debug $ show ("xferStatus writeResult :", cmd, writeResult)
  (retMsg,retStatus) <- readBulkSTL
  debugSTL Debug $ show ("xferStatus readResult : ", retStatus, BS.unpack retMsg)
  return (retMsg, retStatus)

xferBulkWrite :: Cmd -> BS.ByteString -> STL ()
xferBulkWrite cmd block = do
  writeResult1 <- writeCMD cmd
  debugSTL Debug $ show ("xferBulkWrite : ", cmd, writeResult1)
  writeResult2 <- ReaderT $ \STLinkEnv {..} -> do
      liftIO $ writeBulk block
  debugSTL Debug $ show ("xferBulkWrite result : ", writeResult2)

xfer :: Cmd -> STL BS.ByteString
xfer cmd = do
  (ret,err) <- xferStatus cmd
  case err of
    Right Completed -> return ret
    Right TimedOut  -> do
      let msg = "xfer (" ++ show cmd ++ ") : timeout"
      debugSTL Error msg
      error msg
    Left usbExcept -> do
      let msg = "xfer : USB exception : " ++ show usbExcept
      debugSTL Error msg
      error msg

-- todo xferRetry is expected to fail
-- it should not throw an exception but return an error
xferRetry :: Cmd -> STL BS.ByteString
xferRetry cmd = loop 8 10000
  where
    exit :: Show x => x -> STL BS.ByteString
    exit x = do
       debugSTL Error (show x)
       error $ show x

    loop :: Int -> Int -> STL BS.ByteString
    loop 0 _ = exit ("xferRetry giving up after retry:", cmd)
    loop n d = do
       (msg,usbStatus) <- xferStatus cmd
       case usbStatus of
          Left err -> exit ("xferRetry usb error ",err) -- todo
          Right Completed ->  case toStatus $ BS.head msg of
              SWD_AP_WAIT -> retry
              SWD_DP_WAIT -> retry
              DEBUG_ERR_OK -> return msg
              dongleStatus -> exit ("xferRetry dongle error ",dongleStatus)
          Right other -> exit ("xferRetry usb error ",other)
       where
         retry = do
           debugSTL Warn ("xferRetry retry after delay ("++ show cmd ++")")
           liftIO $ threadDelay d
           loop (n-1) (d*2)

xferReadTrace :: STL (BS.ByteString, Either USBException Status)
xferReadTrace = do
  debugSTL Debug $ show "xferReadTrace"
  (retMsg, retStatus) <- readBulkSTL
  debugSTL Debug $ show ("xferReadTrace return : ", retStatus, BS.unpack retMsg)
  return (retMsg, retStatus)
