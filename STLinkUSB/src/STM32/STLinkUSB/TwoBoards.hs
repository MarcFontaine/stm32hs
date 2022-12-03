----------------------------------------------------------------------------
-- |
-- Module      :  STM32.STLinkUSB.TwoBoards
-- Copyright   :  (c) Marc Fontaine 2017-2022
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Using two Boards/Dongles in parallel.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module STM32.STLinkUSB.TwoBoards
where

import System.USB
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import STM32.STLinkUSB.Env
import STM32.STLinkUSB.Commands
import STM32.STLinkUSB.Dongle
import STM32.STLinkUSB.CortexM

type STLTT m a = ReaderT (STLinkEnv,STLinkEnv) m a

runSTLinkAB :: STLTT IO a  -> IO a
runSTLinkAB
 = runSTLinkAB' (defaultDebugLogger, defaultDebugLogger)
   . runReaderT

runSTLinkAB_verbose :: STLTT IO a  -> IO a
runSTLinkAB_verbose
  = runSTLinkAB' (verboseDebugLogger, verboseDebugLogger)
    . runReaderT

runSTLinkAB' ::
     (Logger, Logger)
  -> ((STLinkEnv, STLinkEnv) -> IO a)
  -> IO a
runSTLinkAB' (loggerA, loggerB) action = do
  ctx <- newCtx
  setDebug ctx PrintWarnings
  list <- findUSBDevices ctx defaultSTLProductID
  let (deviceA,deviceB) = case list of
          []  -> error "no STLink dongle found"
          [_] -> error "just one STLink dongle found"
          [a,b] -> (a,b)
          (_:_:_:_) -> error "more two one STLink dongle found"
  (_,_,rxA,txA,traceA) <- findEndpoints ctx deviceA
  (_,_,rxB,txB,traceB) <- findEndpoints ctx deviceB
  let
     preEnvA handleA = STLinkEnv {
       usbCtx      = ctx
      ,rxEndpoint  = rxA
      ,txEndpoint  = txA
      ,traceEndpoint = traceA
      ,deviceHandle  = handleA
      ,dongleAPI     = APIV2
      ,debugLogger   = taggedLogger "A" loggerA
      }
     preEnvB handleB = STLinkEnv {
       usbCtx      = ctx
      ,rxEndpoint  = rxB
      ,txEndpoint  = txB
      ,traceEndpoint = traceB
      ,deviceHandle  = handleB
      ,dongleAPI     = APIV2
      ,debugLogger   = taggedLogger "B" loggerB
      }
  runSTLinkWithAB (deviceA, deviceB) (preEnvA, preEnvB) action

runSTLinkWithAB ::
      (Device, Device)
   -> (DeviceHandle -> STLinkEnv, DeviceHandle -> STLinkEnv)
   -> ((STLinkEnv, STLinkEnv) -> IO a)
   -> IO a
runSTLinkWithAB (deviceA, deviceB) (preEnvA, preEnvB) action
  =  withUSB deviceA $ \deviceHandleA ->
      withUSB deviceB $ \deviceHandleB ->
        action (preEnvA deviceHandleA, preEnvB deviceHandleB)

taggedLogger :: String -> Logger -> Logger
taggedLogger tag logger loglevel msg
  = logger loglevel (tag++":"++msg)

boardA :: STLT IO a  -> STLTT IO a
boardA action = do
  env <- asks fst
  liftIO $ runReaderT action env

boardB :: STLT IO a  -> STLTT IO a
boardB action = do
  env <- asks snd
  liftIO $ runReaderT action env

testTwoBoards :: IO ()
testTwoBoards = runSTLinkAB_verbose $ do
  boardA initDongle
  _ <- boardA readCpuID
  boardB initDongle
