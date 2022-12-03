----------------------------------------------------------------------------
-- |
-- Module      :  STM32.STLinkUSB.Env
-- Copyright   :  (c) Marc Fontaine 2017-2022
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The STLT Monad is just a reader transformer of STLinkEnv.


{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module STM32.STLinkUSB.Env
where

import Control.Monad.Trans.Reader
import Control.Monad.IO.Class

import STM32.STLinkUSB.USB
import STM32.STLinkUSB.Commands (API(..))

type STLT m a = ReaderT STLinkEnv m a
type STL a = forall m. MonadIO m => ReaderT STLinkEnv m a

-- runSTLink :: STLT IO a  -> IO a
-- runSTLink = runSTLink' defaultDebugLogger . runReaderT

-- runSTLink_verbose :: STLT IO a  -> IO a
-- runSTLink_verbose = runSTLink' verboseDebugLogger . runReaderT

runSTLink'
  :: Logger
  -> FindEndpoint ep
  -> WithEndpoint ep a
  -> (STLinkEnv -> IO a)
  -> IO a
runSTLink' debugLogger findEndpoint withEndpoint action = do
   ep <- findEndpoint
   withEndpoint ep $ \(readBulk, writeBulk) -> action STLinkEnv {..}
  where
    dongleAPI = APIV2

data STLinkEnv = STLinkEnv {
    debugLogger :: Logger
  , dongleAPI  :: API
  , readBulk  :: ReadBulk
  , writeBulk :: WriteBulk
  }

asksDongleAPI :: STL API
asksDongleAPI = asks dongleAPI

data LogLevel = Debug | Info | Warn | Error deriving (Show,Eq,Ord)
type Logger = LogLevel -> String -> IO ()

debugSTL :: LogLevel -> String -> STL ()
debugSTL ll msg = do
  logger <- asks debugLogger
  liftIO $ logger ll msg

defaultDebugLogger :: Logger
defaultDebugLogger logLevel msg = case logLevel of
  Debug -> return ()
  Info  -> return ()
  _ -> putStrLn (show logLevel ++ " : " ++ msg )

verboseDebugLogger :: Logger
verboseDebugLogger logLevel msg
  = putStrLn (show logLevel ++ " : " ++ msg )
