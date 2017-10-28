----------------------------------------------------------------------------
-- |
-- Module      :  STM32.STLinkUSB.Dongle
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Functions for initializing, reseting and mode-change of the STLink dongle.

{-# LANGUAGE RankNTypes #-}
module STM32.STLinkUSB.Dongle
where
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL (fromStrict)
import Data.Binary
import Data.Binary.Get
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import System.USB (Status(..))

import STM32.STLinkUSB.Commands
import STM32.STLinkUSB.Env
import STM32.STLinkUSB.USBXfer

-- | Init the dongle and set debug mode.
-- A Haskell translation of the same function in the openocd library.

initDongle :: STL ()
initDongle = do
  debugSTL Debug "starting initDongle"
  v <- readVersion
  debugSTL Info ("dongle version : " ++ show v)
  devMode <- readCurrentMode
  case devMode of
    DEV_DFU_MODE   -> modeLeave MODE_DFU
    DEV_DEBUG_MODE -> modeLeave MODE_DEBUG_SWD
    DEV_SWIM_MODE  -> modeLeave MODE_DEBUG_SWIM
    _ -> return ()
  _nMode <- readCurrentMode
  when (_nMode /= DEV_DFU_MODE) $ do
    voltage <- readVoltage
    debugSTL Info ("dongle voltage : " ++ show voltage)
  debugSTL Info "entering SWD Mode // connection to controller"
  modeEnter MODE_DEBUG_SWD
  newMode <- readCurrentMode
  when (newMode /= DEV_DEBUG_MODE) $ do
    let err = ("cannot set dongle mode DEV_DEBUG_MODE. Mode is : "++ show newMode)
    debugSTL Error err                    
    error err
  return ()

reset :: STL ()
reset = do
  debugSTL Info "resetting dongle"
  api <- asks dongleAPI
  void $ xferRetry (DEBUG_COMMAND $ RESETSYS api)

readVersion :: STL Version
readVersion = do
  debugSTL Debug "reading dongle version"
  msg <- xfer GET_VERSION
  return $ decode $ BSL.fromStrict msg
  
readVoltage :: STL Float
readVoltage = do
  debugSTL Debug "reading dongle voltage"
  msg <- xfer GET_TARGET_VOLTAGE
  let    (a,b) = runGet ((,) <$> getWord32le <*> getWord32le) $ BSL.fromStrict msg
  return $
    2.4 * (realToFrac b) / (realToFrac a)

readCurrentMode :: STL DevMode
readCurrentMode = do
  debugSTL Debug "reading dongle mode"
  msg <- xfer GET_CURRENT_MODE
  let mode = toEnum $ fromIntegral $ BS.head msg
  debugSTL Debug $ "dongle mode : " ++ show mode
  return mode

data Mode
  = MODE_DFU
  | MODE_MASS
  | MODE_DEBUG_JTAG
  | MODE_DEBUG_SWD
  | MODE_DEBUG_SWIM
  deriving (Show,Eq,Ord,Enum)
  
modeEnter :: Mode ->STL ()
modeEnter mode = do
  api <- asks dongleAPI
  case mode of
    MODE_DEBUG_JTAG -> void $ xferRetry $ DEBUG_COMMANDs [ ENTER api , ENTER_JTAG ]
    MODE_DEBUG_SWD  -> void $ xferRetry $ DEBUG_COMMANDs [ ENTER api , ENTER_SWD  ]
    MODE_DEBUG_SWIM -> void $ xferRetry $ SWIM_COMMAND SWIM_ENTER
    MODE_DFU   -> return ()
    MODE_MASS  -> return ()

modeLeave :: Mode -> STL ()
modeLeave mode = do
  case mode of
    MODE_DEBUG_JTAG -> xferCheck $ DEBUG_COMMAND EXIT
    MODE_DEBUG_SWD  -> xferCheck $ DEBUG_COMMAND EXIT
    MODE_DEBUG_SWIM -> xferCheck $ SWIM_COMMAND SWIM_EXIT
    MODE_DFU        -> xferCheck $ DFU_COMMAND_EXIT
    _ -> return ()
  where
      xferCheck cmd = do
        (_ret,err) <- xferStatus cmd
        case err of
           Right TimedOut  -> return () -- this is what happens : timeout
           Right Completed -> return () -- this case was not seen
           Left usbExcept -> do
             let msg = "leaveMode : USB exception : " ++ show usbExcept
             debugSTL Error msg
             error msg
             
writeDebugReg :: Word32 -> Word32 -> STL()
writeDebugReg addr val = do
  api <- asks dongleAPI
  void $ xfer (DEBUG_COMMAND $ WRITEDEBUGREG api addr val)

dumpTrace :: STL ()
dumpTrace = forever $ do
   (msg,err) <- xferReadTrace
   liftIO $ print ("trace: ",err,msg)

