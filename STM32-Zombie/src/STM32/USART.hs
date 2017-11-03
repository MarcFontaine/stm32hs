----------------------------------------------------------------------------
-- |
-- Module      :  STM32.USART
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- USART (Serial Port)

{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module STM32.USART
where

import Device
import STM32.MachineInterface
import STM32.Utils
import STM32.GPIO as GPIO
import qualified STM32.RCC as RCC

import Control.Monad
import Data.Word
       
data Config = Config  {
     _baudRate   :: BaudRate
   , _wordLength :: WordLength
   , _stopBits   :: StopBits
   , _parity     :: Parity
   , _mode       :: Mode
   , _hardwareFlowControl :: HardwareFlowControl
   } deriving (Show)

{-
Only USART1 is clocked with PCLK2 (72 MHz Max). Other USARTs are clocked with
PCLK1 (36 MHz Max).

todo :: fix the baudrate stuff--
baudrate depends on clock

-}


defaultConfig :: Config
defaultConfig = Config  {
     _baudRate   = BaudRateRegisterValue 625 -- 115200 @ 72Mhz
   , _wordLength = Eight
   , _stopBits   = One
   , _parity     = No
   , _mode       = RxTx
   , _hardwareFlowControl = None
   }
   
data WordLength = Eight | Nine deriving Show
instance ToBit WordLength where
  toBit Eight = False
  toBit Nine  = True

data StopBits = Zero5 | One | One5 | Two deriving Show
instance RegisterField StopBits where
  toBits b = case b of
     One   -> "00"
     Zero5 -> "01"
     Two   -> "10"
     One5  -> "11"
  toField = const CR2_STOP
  
data Parity   = No | Even | Odd deriving Show
data Mode = Rx | Tx | RxTx deriving Show
data HardwareFlowControl = None | RTS | CTS | RTS_CTS | NA deriving (Eq,Show)
data BaudRate = BaudRateRegisterValue {getBRR :: Word16} deriving Show 

deInit :: Peripheral -> MI ()
deInit = RCC.peripheralResetToggle

init :: Peripheral -> Config -> MI ()
init p conf = do
  let write field rs = bitWrite p field rs
  
  fieldWrite p $ _stopBits conf

  write CR1_PCE $ case _parity conf of
        No -> False
        Even -> False        
        Odd  -> True

  write CR1_PS $ case _parity conf of
        Even -> False
        _ -> True

  write CR1_M $ _wordLength conf

  write CR1_TE $ case _mode conf of
    Tx   -> True 
    Rx   -> False
    RxTx -> True

  write CR1_RE $ case _mode conf of
    Tx   -> False
    Rx   -> True
    RxTx -> True

  when (_hardwareFlowControl conf /= NA) $ do
      write CR3_RTSE $ case _hardwareFlowControl conf of
        RTS     -> True
        RTS_CTS -> True
        _       -> False
      write CR3_CTSE $ case _hardwareFlowControl conf of
        CTS     -> True
        RTS_CTS -> True
        _ -> False

  pokeReg p BRR $ fromIntegral $ getBRR $ _baudRate conf

sendWord8 :: Peripheral -> Word8 -> MI ()
sendWord8 p b = pokeReg p DR $ fromIntegral b

enable :: Peripheral -> MI ()
enable p = bitSet p CR1_UE

disable :: Peripheral -> MI ()
disable p = bitReset p CR1_UE 

data UartPort = UartPort {
   _UartPeripheral  :: Peripheral
  ,_UartTXWire :: GPIO.Wire
  ,_UartRXWire  :: GPIO.Wire
  ,_UartIsAlternativeMapping :: Bool
  }

stm32F103_UartPort1 :: UartPort
stm32F103_UartPort1 = UartPort {
   _UartPeripheral  = USART1
  ,_UartTXWire = (GPIOA,Pin_9)
  ,_UartRXWire  = (GPIOA,Pin_10)
  ,_UartIsAlternativeMapping = False
  }

stm32F103_UartPort2 :: UartPort
stm32F103_UartPort2 = UartPort {
   _UartPeripheral  = USART2
  ,_UartTXWire = (GPIOA,Pin_2)
  ,_UartRXWire = (GPIOA,Pin_3)
  ,_UartIsAlternativeMapping = False
  }

stm32F103_UartPort3 :: UartPort
stm32F103_UartPort3 = UartPort {
   _UartPeripheral  = USART3
  ,_UartTXWire = (GPIOB,Pin_10)
  ,_UartRXWire  = (GPIOB,Pin_11)
  ,_UartIsAlternativeMapping = False
  }


configure :: UartPort -> Config -> MI () 
configure UartPort {..} config = do
  STM32.USART.deInit _UartPeripheral
  RCC.peripheralClockOn _UartPeripheral
  RCC.peripheralClockOn $ fst _UartRXWire
  RCC.peripheralClockOn AFIO

  GPIO.pinMode _UartTXWire (AlternateOutPushPull MHz_2)
  GPIO.pinMode _UartRXWire InputFloating

  STM32.USART.enable _UartPeripheral
  STM32.USART.init _UartPeripheral config
