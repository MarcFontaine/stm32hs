----------------------------------------------------------------------------
-- |
-- Module      :  STM32.SPI
-- Copyright   :  (c) Marc Fontaine 2017-2022
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The SPI peripheral.

{-# LANGUAGE OverloadedStrings #-}
module STM32.SPI
where

import Device
import STM32.MachineInterface
import STM32.Utils
import qualified STM32.RCC as RCC

import Data.Word

data Config = Config {
    _direction   :: Direction
  , _mode        :: Mode
  , _dataSize    :: DataSize
  , _CPOL        :: ClockPolarity
  , _CPHA        :: ClockPhase
  , _NSS         :: SlaveSelect
  , _baudRatePrescaler :: BaudPrescaler
  , _firstBit          :: FirstBit
  , _CRCPolynomial     :: Word16
  } deriving Show

defaultConfig :: Config
defaultConfig = Config {
    _direction   = Two_Lines_FullDuplex
  , _mode        = Slave
  , _dataSize    = Eight
  , _CPOL        = Low
  , _CPHA        = OneEdge
  , _NSS         = Hard
  , _baudRatePrescaler = Prescaler_2
  , _firstBit          = MSB
  , _CRCPolynomial     = 7
  }

data Direction =
    Two_Lines_FullDuplex --    ((u16)0x0000)
  | Two_Lines_RxOnly     --    ((u16)0x0400)
  | One_Line_Rx          --   ((u16)0x8000)
  | One_Line_Tx          --   ((u16)0xC000)
  deriving (Show)

data Mode = Master | Slave deriving Show
data DataSize = Eight | Sixteen deriving Show
data ClockPolarity = Low | High deriving Show
data ClockPhase    = OneEdge | TwoEdge deriving Show
data SlaveSelect   = Soft | Hard deriving Show
data BaudPrescaler =
    Prescaler_2
  | Prescaler_4
  | Prescaler_8
  | Prescaler_16
  | Prescaler_32
  | Prescaler_64
  | Prescaler_128
  | Prescaler_256
  deriving Show

instance RegisterField BaudPrescaler where
  toBits b = case b of
     Prescaler_2    -> "000"
     Prescaler_4    -> "001"
     Prescaler_8    -> "010"
     Prescaler_16   -> "011"
     Prescaler_32   -> "100"
     Prescaler_64   -> "101"
     Prescaler_128  -> "110"
     Prescaler_256  -> "111"
  toField = const CR1_BR

data FirstBit = MSB | LSB deriving Show

deInit :: Peripheral -> MI ()
deInit = RCC.peripheralResetToggle

init :: Peripheral -> Config -> MI ()
init p conf = do
  let write field rs = bitWrite p field rs

  write CR1_MSTR $ case _mode conf of
        Slave  -> False
        Master -> True

  write CR1_SSI $ case _mode conf of
        Slave  -> False
        Master -> True

  write CR1_DFF $ case _dataSize conf of
        Eight   -> False
        Sixteen -> True

  write CR1_CPOL $ case _CPOL conf of
        Low  -> False
        High -> True

  write CR1_CPHA $ case _CPHA conf of
        OneEdge -> False
        TwoEdge -> True

  write CR1_SSM $ case _NSS conf of
        Hard -> False
        Soft -> True

  fieldWrite p $ _baudRatePrescaler conf

  write CR1_LSBFIRST $ case _firstBit conf of
        MSB -> False
        LSB -> True

  pokeReg p CRCPR $ fromIntegral $ _CRCPolynomial conf

enable :: Peripheral -> MI ()
enable p = bitSet p CR1_SPE

disable :: Peripheral -> MI ()
disable p = bitReset p CR1_SPE

sendData8 :: Peripheral -> Word8 -> MI ()
sendData8 p b = pokeReg p DR $ fromIntegral b

sendData :: Peripheral -> Word16 -> MI ()
sendData p b = pokeReg p DR $ fromIntegral b

receiveData8 :: Peripheral -> MI Word8
receiveData8 p = fromIntegral <$> peekReg p DR

receiveData :: Peripheral -> MI Word16
receiveData p = fromIntegral <$> peekReg p DR

ssOutputCmd :: Peripheral -> Bool -> MI ()
ssOutputCmd p = bitWrite p CR2_SSOE
