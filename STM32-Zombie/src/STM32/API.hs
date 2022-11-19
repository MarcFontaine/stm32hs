----------------------------------------------------------------------------
-- |
-- Module      :  STM32.API
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The general part of the API.
-- The module for the peripheral (GPIO, USART,ADC,..) has to be imported separately.

module STM32.API
(
   module STM32.MachineInterface
 , module STLinkUSB
 , module STM32.RCC
 , module Data.Word
 , module Data.Bits
 , module Device
 , module STM32.Utils
)

where
import Data.Word
import Data.Bits

import Device
import STM32.MachineInterface
import STM32.Utils
import STM32.STLinkUSB as STLinkUSB hiding (resetHalt)
import STM32.RCC (setDefaultClocks , peripheralClockOn
            , peripheralClockOff, peripheralResetToggle)

