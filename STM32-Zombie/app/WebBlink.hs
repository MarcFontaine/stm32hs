module WebBlink
where
import Control.Monad
import System.FilePath

import Paths_STM32_Zombie

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
  debug "debugmessage"
  timestamp
  return w # set title "WebBlink"
  ledOn <- ledButton True
  ledOff <- ledButton False
  getBody w #+ [
        UI.div #. "wrap" #+ greet
      , UI.div #. "wrap" #+ [ connectDeviceButton ]
      , UI.div #. "wrap" #+ [element ledOn]
      , UI.div #. "wrap" #+ [element ledOff ]        
      ]
  liftIO $ putStrLn "OK"
  where
    fkt :: Int -> JSFunction Int
    fkt = ffi "%1"


--    printDevice :: JS.JSObject -> JSFunction String
--    printDevice = ffi "

greet :: [UI Element]
greet =
    [ UI.h1  #+ [string "STM32-Zombie blink LED!"]
    , UI.div #+ [string "Try the buttons below."]
    ]


ledButton :: Bool -> UI Element
ledButton val = do
  button <- UI.button #. "button" #+ [string $  show val]
  on UI.click button $ \_ -> do
        liftIO $ putStr $ show ("button", val)
  return button


connectDeviceButton :: UI Element
connectDeviceButton = do
  button <- UI.button #. "button" #+ [string "Connect STLink Device"]
  on UI.click button $ \_ -> do
        void $ callFunction getDevice
  return button

getDevice :: JSFunction JS.JSObject
getDevice = ffi "navigator.usb.requestDevice({ filters: [{ vendorId: 0x0483 }] })"
