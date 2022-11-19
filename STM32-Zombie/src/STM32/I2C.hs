----------------------------------------------------------------------------
-- |
-- Module      :  STM32.GPIO
-- Copyright   :  (c) Marc Fontaine 2017-2022
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Untested // Work in progress
{-# LANGUAGE OverloadedStrings #-}
module STM32.I2C
where

import Device
import STM32.MachineInterface
import STM32.Utils
import qualified STM32.RCC as RCC

import Data.Word
import Data.Bits

data Config = Config {
    _mode           :: Mode
  , _dutyCycle      :: DutyCycle
  , _ownAddress1    :: Word16
  , _ack            :: Bool
  , _acknowledgedAddress :: AcknowledgedAddress
  , _clocks         :: Clocks
  } deriving Show


defaultConfig :: Config
defaultConfig = Config {
    _mode           = I2C
  , _dutyCycle      = DutyCycle_2
  , _ownAddress1    = 0
  , _ack            = False
  , _acknowledgedAddress = SevenBit
  , _clocks     = defaultClocks
  }

data Mode = I2C | SMBusDevice | SMBusHost deriving Show
data DutyCycle = DutyCycle_16_9 | DutyCycle_2 deriving Show
data Direction = Transmitter | Receiver  deriving Show
data AcknowledgedAddress = SevenBit | TenBit deriving Show

data Clocks = Clocks {
    _freq  :: Word32
   ,_ccr   :: Word32
   ,_trise :: Word32
   } deriving Show

defaultClocks :: Clocks
defaultClocks = Clocks {_freq = 36,_ccr=0,_trise=0}

deInit :: Peripheral -> MI ()
deInit = RCC.peripheralResetToggle

init :: Peripheral -> Config -> MI ()
init p conf = do
  cr2 <- peekReg p CR2
  pokeReg p CR2 $ ((cr2 .&. 0xffffffe0) .|. fromIntegral (_freq  $ _clocks conf))

  disable p
  pokeReg p TRISE $ _trise $ _clocks conf
  pokeReg p CCR   $ _ccr   $ _clocks conf
  enable p
  let write field rs = bitWrite p field rs

  write CR1_SMBUS $ case _mode conf of
     I2C         -> False
     SMBusDevice -> True
     SMBusHost   -> True

  write CR1_SMBTYPE $ case _mode conf of
     I2C         -> False
     SMBusDevice -> False
     SMBusHost   -> True

  write CR1_ACK $ _ack conf

  oar1 <- peekReg p OAR1
  pokeReg p OAR1
    (     (oar1 .&. 0x000003ff)
      .|. 0x00004000
      .|. fromIntegral ( _ownAddress1 conf)
      .|. (case _acknowledgedAddress conf of
             SevenBit -> 0
             TenBit   -> 0x00008000
          )
    )

enable :: Peripheral -> MI ()
enable p = bitSet p CR1_PE

disable :: Peripheral -> MI ()
disable p = bitReset p CR1_PE
