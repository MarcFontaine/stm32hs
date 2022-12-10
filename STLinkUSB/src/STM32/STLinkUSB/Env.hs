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

type STLT m a = ReaderT (STLinkEnv m) m a
type STL a = forall m. MonadIO m => STLT m a

-- runSTLink :: STLT IO a  -> IO a
-- runSTLink = runSTLink' defaultDebugLogger . runReaderT

-- runSTLink_verbose :: STLT IO a  -> IO a
-- runSTLink_verbose = runSTLink' verboseDebugLogger . runReaderT

runSTLink'
  :: Monad m
  => Logger m
  -> FindEndpoint m ep
  -> WithEndpoint m ep a
  -> (STLinkEnv m -> m a)
  -> m a
runSTLink' debugLogger findEndpoint withEndpoint action = do
   ep <- findEndpoint
   withEndpoint ep $ \(readBulk, writeBulk) -> action STLinkEnv {..}
  where
    dongleAPI = APIV2

data STLinkEnv m = STLinkEnv {
    debugLogger :: Logger m
  , dongleAPI  :: API
  , readBulk  :: ReadBulk m
  , writeBulk :: WriteBulk m
  }

asksDongleAPI :: STL API
asksDongleAPI = asks dongleAPI

data LogLevel = Debug | Info | Warn | Error deriving (Show,Eq,Ord)
type Logger m = Monad m => LogLevel -> String -> m ()

debugSTL :: Monad m => LogLevel -> String -> STLT m ()
debugSTL ll msg = do
  logger <- asks debugLogger
  ReaderT $ const $ logger ll msg

defaultDebugLogger :: MonadIO m => Logger m
defaultDebugLogger logLevel msg = case logLevel of
  Debug -> return ()
  Info  -> return ()
  _ -> liftIO $ putStrLn (show logLevel ++ " : " ++ msg )

verboseDebugLogger :: MonadIO m => Logger m
verboseDebugLogger logLevel msg
  = liftIO $ putStrLn (show logLevel ++ " : " ++ msg )
