----------------------------------------------------------------------------
-- |
-- Module      :  STM32.MachineInterface
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
-- 
-- At the moment there is just one implementation for the MachineInterface
-- namely STM32.MachineInterfaceSTLinkUSB.
-- All direct communication with the microcontroller runs through this API.
-- 

module STM32.MachineInterface
(
   MI
  ,runMI
  ,initMI
  ,resetHalt
  ,peek_w16
  ,poke_w16
  ,peek_w32
  ,poke_w32
)        
where
import STM32.MachineInterfaceSTLinkUSB

