----------------------------------------------------------------------------
-- |
-- Module      :  F401.API
-- Copyright   :  (c) Marc Fontaine 2025
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The general part of the API.
-- The module for the peripheral (GPIO, USART,ADC,..) has to be imported separately.

module F401.API
(
   module F401.MachineInterface
 , module F401.Utils
-- , module F401.RCC
 , module STLinkUSB
 , module Data.Word
 , module Data.Bits
 , module Device
)

where
import Data.Word
import Data.Bits

import Device
import F401.MachineInterface
import F401.Utils
import STM32.STLinkUSB as STLinkUSB hiding (resetHalt)
--import F401.RCC (setDefaultClocks , peripheralClockOn
--            , peripheralClockOff, peripheralResetToggle)

