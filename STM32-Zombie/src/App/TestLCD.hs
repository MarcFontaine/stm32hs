----------------------------------------------------------------------------
-- |
-- Module      :  App.LCDDemo
-- License     :  BSD3
-- 
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The LCDDemo module has been copied from the hArduino package.
-- This is the System.Hardware.Arduino.Parts.TestLCD module
-- with some minor adaption for STM32.
-- System.Hardware.Arduino.Parts.TestLCD is copyright by Levent Erkok
-- 

module App.TestLCD
where
import App.LCD

import STM32.API
import STM32.GPIO as GPIO
import Control.Monad.IO.Class
import Data.Char           (isSpace)
       
       
port :: Peripheral
port = GPIOB

hitachi :: LCDController
hitachi = Hitachi44780 {
                       lcdRS  = (port,GPIO.Pin_10)
                     , lcdEN  = (port,GPIO.Pin_2)
                     , lcdD4  = (port,GPIO.Pin_13)
                     , lcdD5  = (port,GPIO.Pin_14)
                     , lcdD6  = (port,GPIO.Pin_11)
                     , lcdD7  = (port,GPIO.Pin_12)
                     , lcdRows  = 2
                     , lcdCols  = 16
                     , dotMode5x10 = True
                     }

-- | The happy glyph. See 'lcdCreateSymbol' for details on how to create new ones.
happy :: [String]
happy = [ "     "
        , "@   @"
        , "     "
        , "     "
        , "@   @"
        , " @@@ "
        , "     "
        , "     "
        ]

-- | The sad glyph. See 'lcdCreateSymbol' for details on how to create new ones.
sad :: [String]
sad = [ "     "
      , "@   @"
      , "     "
      , "     "
      , "     "
      , " @@@ "
      , "@   @"
      , "     "
      ]

-- | Access the LCD connected to Arduino, making it show messages
-- we read from the user and demonstrate other LCD control features offered
-- by hArduino.
lcdDemo :: IO ()
lcdDemo = runMI $ do
              initMI
              resetHalt
              peripheralClockOn port
              lcd <- lcdRegister hitachi
              happySymbol <- lcdCreateSymbol lcd happy
              sadSymbol   <- lcdCreateSymbol lcd sad
              lcdHome lcd
              liftIO $ do putStrLn "Hitachi controller demo.."
                          putStrLn ""
                          putStrLn "Looking for an example? Try the following sequence:"
                          putStrLn "    cursor 5 0"
                          putStrLn "    happy"
                          putStrLn "    write _"
                          putStrLn "    happy"
                          putStrLn "    flash 5"
                          putStrLn ""
                          putStrLn "Type ? to see all available commands."
              let repl = do liftIO $ putStr "LCD> "
                            m <- liftIO getLine
                            case words m of
                              []       -> repl
                              ["quit"] -> return ()
                              (cmd:_)    -> case cmd `lookup` commands of
                                              Nothing        -> do liftIO $ putStrLn $ "Unknown command '" ++ cmd ++ "', type ? for help."
                                                                   repl
                                              Just (_, _, c) -> do c lcd (dropWhile isSpace (drop (length cmd) m)) (happySymbol, sadSymbol)
                                                                   repl
              repl
  where help = liftIO $ do let (cmds, args, hlps) = unzip3 $ ("quit", "", "Quit the demo") : [(c, a, h) | (c, (a, h, _)) <- commands]
                               clen = 1 + maximum (map length cmds)
                               alen = 8 + maximum (map length args)
                               pad l s = take l (s ++ repeat ' ')
                               line (c, a, h) = putStrLn $ pad clen c ++ pad alen a ++ h
                           mapM_ line $ zip3 cmds args hlps
        arg0 f _   [] _ = f
        arg0 _ _   a  _ = liftIO $ putStrLn $ "Unexpected arguments: " ++ show a
        arg1 f lcd [] _ = f lcd
        arg1 _ _   a  _ = liftIO $ putStrLn $ "Unexpected arguments: " ++ show a
        arg2 f lcd a  _ = f lcd a
        arg3            = id
        grabNums n a f  = case [v | [(v, "")] <- map reads (words a)] of
                            vs | length vs /= n -> liftIO $ putStrLn $ "Need " ++ show n ++ " numeric parameter" ++ if n == 1 then "." else "s."
                            vs                  -> f vs
        symbol isHappy lcd _ (h, s) = lcdWriteSymbol lcd (if isHappy then h else s)
        cursor lcd a = grabNums 2 a (\[col, row] -> lcdSetCursor lcd (col, row))
        flash  lcd a = grabNums 1 a (\[n] -> lcdFlash lcd n 500)
        code   lcd a = grabNums 1 a (\[n] -> do lcdClear lcd
                                                lcdHome lcd
                                                lcdWriteSymbol lcd (lcdInternalSymbol n)
                                                lcdWrite lcd $ " (Code: " ++ show n  ++ ")")
        scroll toLeft lcd a = grabNums 1 a (\[n] -> do let scr | toLeft = lcdScrollDisplayLeft
                                                               | True   = lcdScrollDisplayRight
                                                       sequence_ $ concat $ replicate n [scr lcd, delay 500])
        commands = [ ("?",           ("",        "Display this help message",   arg0 help))
                   , ("clear",       ("",        "Clear the LCD screen",        arg1 lcdClear))
                   , ("write",       ("string",  "Write to the LCD",            arg2 lcdWrite))
                   , ("home",        ("",        "Move cursor to home",         arg1 lcdHome))
                   , ("cursor",      ("col row", "Move cursor to col row",      arg2 cursor))
                   , ("scrollOff",   ("",        "Turn off auto-scroll",        arg1 lcdAutoScrollOff))
                   , ("scrollOn",    ("",        "Turn on auto-scroll",         arg1 lcdAutoScrollOn))
                   , ("scrollLeft",  ("n",       "Scroll left by n chars",      arg2 (scroll True)))
                   , ("scrollRight", ("n",       "Scroll right by n char",      arg2 (scroll False)))
                   , ("leftToRight", ("",        "Set left to right direction", arg1 lcdLeftToRight))
                   , ("rightToLeft", ("",        "Set left to right direction", arg1 lcdRightToLeft))
                   , ("blinkOn",     ("",        "Set blinking ON",             arg1 lcdBlinkOn))
                   , ("blinkOff",    ("",        "Set blinking ON",             arg1 lcdBlinkOff))
                   , ("cursorOn",    ("",        "Display the cursor",          arg1 lcdCursorOn))
                   , ("cursorOff",   ("",        "Do not display the cursor",   arg1 lcdCursorOff))
                   , ("displayOn",   ("",        "Turn the display on",         arg1 lcdDisplayOn))
                   , ("displayOff",  ("",        "Turn the display off",        arg1 lcdDisplayOff))
                   , ("flash",       ("n",       "Flash the display n times",   arg2 flash))
                   , ("happy",       ("",        "Draw a smiling face",         arg3 (symbol True)))
                   , ("sad",         ("",        "Draw a sad face",             arg3 (symbol False)))
                   , ("code",        ("n",       "Write symbol with code n",    arg2 code))
                   ]
