module WebBlink
where
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Concurrent
import Control.Concurrent.MVar
import System.FilePath
import qualified Data.ByteString as BS

import Paths_STM32_Zombie
import STM32.STLinkUSB.Commands hiding (Status)
import STM32.STLinkUSB.Env
import STM32.STLinkUSB.USB

import STM32.API
import STM32.GPIO as GPIO

import App.Blink

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Foreign.JavaScript as JS


{-----------------------------------------------------------------------------
    Buttons
------------------------------------------------------------------------------}

main :: IO ()
main = do
  static <- getStaticDir
  startGUI defaultConfig { jsStatic = Just static } setup

getStaticDir :: IO FilePath
getStaticDir = (</> "samples/static") `liftM` getDataDir

setup :: Window -> UI ()
setup w = void $ do
  setCallBufferMode NoBuffering
  stLinkRef <- liftIO $ newEmptyMVar
  debug "debugmessage"
  timestamp
  return w # set title "WebBlink"
  ledOn <- ledButton stLinkRef True
  ledOff <- ledButton stLinkRef False
  getBody w #+ [
        UI.div #. "wrap" #+ greet
      , UI.div #. "wrap" #+ [ connectDeviceButton stLinkRef ]
      , UI.div #. "wrap" #+ [ element ledOn ]
      , UI.div #. "wrap" #+ [ element ledOff ] 
      ]
  liftIO $ putStrLn "OK"

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "STM32-Zombie blink LED!"]
    , UI.div #+ [string "Try the buttons below."]
    ]


ledButton :: MVar (STLinkEnv UI) -> Bool -> UI Element
ledButton stLinkEnv val = do
  button <- UI.button #. "button" #+ [string $  show val]
  on UI.click button $ \_ -> do
        liftIO $ putStr $ show ("button", val)
              
  return button

connectDeviceButton :: MVar (STLinkEnv UI) -> UI Element
connectDeviceButton ref = do
  button <- UI.button #. "button" #+ [string "Connect STLink Device"]
  on UI.click button buttonCallback
  return button
 where
  buttonCallback _ = do
    device <- callFunction getDevice
    callFunction $ openDevice device
    callFunction $ selectConf device
    callFunction $ claim device
    inter           
    let stLinkEnv = STLinkEnv {
        debugLogger = \logLevel msg -> liftIO $ putStrLn msg
      , dongleAPI = APIV2
      , readBulk = jsReadBulk device
      , writeBulk = jsWriteBulk device
      }
--    flip runReaderT stLinkEnv $ do
--      initMI
--      resetHalt
    flip runReaderT stLinkEnv $ myBlink led
    liftIO $ putMVar ref stLinkEnv
    return ()
    
getDevice :: JSFunction JS.JSObject
getDevice = ffi "navigator.usb.requestDevice({ filters: [{ vendorId: 0x0483 }] })"

openDevice :: JS.JSObject -> JSFunction ()
openDevice = ffi "%1.then((device) => device.open())"
selectConf :: JS.JSObject -> JSFunction ()
selectConf = ffi "%1.then((device) => device.selectConfiguration(1))"
claim :: JS.JSObject -> JSFunction ()
claim = ffi "%1.then((device) => device.claimInterface(0))"


jsWriteBulk
  :: JS.JSObject
  -> BS.ByteString
  -> UI (Size, Status)
jsWriteBulk device bs = do
  res <- callFunction $ fkt device $ TypedArray bs
  return (BS.length bs, Completed)
  where
    fkt :: JS.JSObject -> TypedArray -> JSFunction JS.JSObject
    fkt = ffi "%1.then((device) => {device.transferOut(2, %2 );})"

jsReadBulk
  :: JS.JSObject
  -> UI (BS.ByteString, Either USBException Status)
jsReadBulk device = do
--  res <- callFunction $ fkt device
  return (BS.empty, Right Completed)
  where
    fkt :: JS.JSObject -> JSFunction JS.JSObject
    fkt = ffi "%1.then((device) => {device.transferIn(1, 64);})"

newtype TypedArray = TypedArray BS.ByteString
instance ToJS TypedArray where
  render (TypedArray bs) = return $ JS.JSCode $ concat [
      "(new Uint8Array("
    , show $ BS.unpack bs
    , "))"
    ]

inter :: MonadIO m => m ()
inter = liftIO $ do
      putStrLn "weiter"
      getLine
      return ()

myBlink :: MonadIO m => Wire -> STLT m ()
myBlink led = do
  let (port,_) = led
  peripheralClockOn port
  pinMode led $ GPOutPushPull MHz_2
--  forever $ do
  pinLow led
  delay 500000
--     pinHigh led
--     delay 500000


led = (GPIOC, Pin_13)
