----------------------------------------------------------------------------
-- |
-- Module      :  STM32.STLinkUSB.USB
-- Copyright   :  (c) Marc Fontaine 2022
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only

-- An abstraction for usb os layers.
-- Possible instantiations: libusb (System.USB), webUSB

module STM32.STLinkUSB.USB
(
    Size
  , Status (..)
  , USBCallbacks (..)
  , USBException  
--  , findDongle
--  , withUSBLayer
)
where

import Control.Exception (SomeException(..))
import qualified Data.ByteString as BS

type USBException = SomeException

data Status = Completed | TimedOut
  deriving (Eq, Read, Show)

type Size = Int

data USBCallbacks = USBCallbacks
  {
    writeBulk :: BS.ByteString -> IO (Size, Status)
  , readBulk  :: IO (BS.ByteString, Either USBException Status)
  }
