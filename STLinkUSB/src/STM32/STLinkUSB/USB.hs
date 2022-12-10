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
  , FindEndpoint
  , WithEndpoint
  , Status (..)
  , USBException
  , ReadBulk
  , WriteBulk
--  , findDongle
--  , withUSBLayer
)
where

import Control.Exception (SomeException(..))
import qualified Data.ByteString as BS

type USBException = SomeException

type ReadBulk m = m (BS.ByteString, Either USBException Status)
type WriteBulk m = BS.ByteString -> m (Size, Status)
type FindEndpoint m a = m a
type WithEndpoint m a b = a -> ((ReadBulk m, WriteBulk m) -> m b) -> m b

data Status = Completed | TimedOut
  deriving (Eq, Read, Show)

type Size = Int

{-
data USBCallbacks = USBCallbacks
  {
    readBulk  :: ReadBulk
  , writeBulk :: WriteBulk
  }
-}
