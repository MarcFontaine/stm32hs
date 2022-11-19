----------------------------------------------------------------------------
-- |
-- Module      :  STM32.DMA
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
--
-- The direct memory access (DMA)
-- controller is one of the coolest features of STM32Fxxx
-- micro controllers.
-- For example, one can sample signals at a fast and precise
-- sampling rate or generate wave-form patterns using DMA transfers.
-- DMA transfers run completely independent and in parallel
-- from the CPU or the Haskell code.

{-# LANGUAGE NoMonomorphismRestriction #-}
module STM32.DMA
where

import Device
import STM32.MachineInterface
import STM32.Utils

import Data.Word

data Channel
  = DMA1_Channel1
  | DMA1_Channel2
  | DMA1_Channel3
  | DMA1_Channel4
  | DMA1_Channel5
  | DMA1_Channel6
  | DMA1_Channel7
  | DMA2_Channel1
  | DMA2_Channel2
  | DMA2_Channel3
  | DMA2_Channel4
  | DMA2_Channel5
  deriving Show

channelToPeripheral :: Channel -> Peripheral
channelToPeripheral ch = case ch of
     DMA1_Channel1 -> DMA1
     DMA1_Channel2 -> DMA1
     DMA1_Channel3 -> DMA1
     DMA1_Channel4 -> DMA1
     DMA1_Channel5 -> DMA1
     DMA1_Channel6 -> DMA1
     DMA1_Channel7 -> DMA1
     DMA2_Channel1 -> DMA2
     DMA2_Channel2 -> DMA2
     DMA2_Channel3 -> DMA2
     DMA2_Channel4 -> DMA2
     DMA2_Channel5 -> DMA2

channelToCCR :: Channel -> Register
channelToCCR ch = case ch of
     DMA1_Channel1 -> CCR1
     DMA1_Channel2 -> CCR2
     DMA1_Channel3 -> CCR3
     DMA1_Channel4 -> CCR4
     DMA1_Channel5 -> CCR5
     DMA1_Channel6 -> CCR6
     DMA1_Channel7 -> CCR7
     DMA2_Channel1 -> CCR1
     DMA2_Channel2 -> CCR2
     DMA2_Channel3 -> CCR3
     DMA2_Channel4 -> CCR4
     DMA2_Channel5 -> CCR5

channelToCNDTR :: Channel -> Register
channelToCNDTR ch = case ch of
     DMA1_Channel1 -> CNDTR1
     DMA1_Channel2 -> CNDTR2
     DMA1_Channel3 -> CNDTR3
     DMA1_Channel4 -> CNDTR4
     DMA1_Channel5 -> CNDTR5
     DMA1_Channel6 -> CNDTR6
     DMA1_Channel7 -> CNDTR7
     DMA2_Channel1 -> CNDTR1
     DMA2_Channel2 -> CNDTR2
     DMA2_Channel3 -> CNDTR3
     DMA2_Channel4 -> CNDTR4
     DMA2_Channel5 -> CNDTR5

channelToCPAR :: Channel -> Register
channelToCPAR ch = case ch of
     DMA1_Channel1 -> CPAR1
     DMA1_Channel2 -> CPAR2
     DMA1_Channel3 -> CPAR3
     DMA1_Channel4 -> CPAR4
     DMA1_Channel5 -> CPAR5
     DMA1_Channel6 -> CPAR6
     DMA1_Channel7 -> CPAR7
     DMA2_Channel1 -> CPAR1
     DMA2_Channel2 -> CPAR2
     DMA2_Channel3 -> CPAR3
     DMA2_Channel4 -> CPAR4
     DMA2_Channel5 -> CPAR5

channelToCMAR :: Channel -> Register
channelToCMAR ch = case ch of
     DMA1_Channel1 -> CMAR1
     DMA1_Channel2 -> CMAR2
     DMA1_Channel3 -> CMAR3
     DMA1_Channel4 -> CMAR4
     DMA1_Channel5 -> CMAR5
     DMA1_Channel6 -> CMAR6
     DMA1_Channel7 -> CMAR7
     DMA2_Channel1 -> CMAR1
     DMA2_Channel2 -> CMAR2
     DMA2_Channel3 -> CMAR3
     DMA2_Channel4 -> CMAR4
     DMA2_Channel5 -> CMAR5

data Config = Config {
   _BufferSize         :: Word16     --number of entries
  ,_Direction          :: Direction
  ,_MemoryBaseAddr     :: Word32
  ,_MemoryDataSize     :: DataSize
  ,_MemoryInc          :: Bool
  ,_Mode               :: Mode
  ,_PeripheralBaseAddr :: Word32
  ,_PeripheralDataSize :: DataSize
  ,_PeripheralInc      :: Bool
  ,_Priority           :: Priority
  } deriving Show

data Direction
  = PeripheralDST | PeripheralSRC | Mem2Mem
  deriving Show

data Priority
  = VeryHigh | High | Medium | Low
  deriving Show

  {-
instance ToBitField Priority where
  toBitField p = case p of
-}

data DataSize
  = Byte | HalfWord | Word
  deriving Show

data Mode
  = Circular | Normal
  deriving Show

instance ToBit Mode where
  toBit Normal = False
  toBit Circular = True


writeCCRxOffset :: ToBit b => Int -> Channel -> Field -> b -> MI()
writeCCRxOffset offset channel field rs
  = bitWriteRaw rs
      (regToAddr (channelToPeripheral channel) $ channelToCCR channel )
      (offset + fieldBitOffset field)

init :: Channel -> Config -> MI ()
init channel config = do
  let
     peri = channelToPeripheral channel
     writeCCRx  = writeCCRxOffset 0 channel
     writeCCRxH = writeCCRxOffset 1 channel
     writeCCRxL = writeCCRx

     poke  = pokeReg peri
     cndtr = channelToCNDTR channel
     cpar = channelToCPAR channel
     cmar = channelToCMAR channel

  writeCCRx CCR1_DIR $ case _Direction config of
     PeripheralSRC -> False
     Mem2Mem       -> True
     PeripheralDST -> True

  writeCCRx CCR1_CIRC $ _Mode config
  writeCCRx CCR1_PINC $ _PeripheralInc config
  writeCCRx CCR1_MINC $ _MemoryInc config

  let (psizeH,psizeL) = case _PeripheralDataSize config of
        Byte     -> (False,False)
        HalfWord -> (False, True)
        Word     -> (True ,False)
  writeCCRxL CCR1_PSIZE psizeL
  writeCCRxH CCR1_PSIZE psizeH

  let (msizeH,msizeL) = case _MemoryDataSize config of
        Byte     -> (False,False)
        HalfWord -> (False, True)
        Word     -> (True ,False)
  writeCCRxL CCR1_MSIZE msizeL
  writeCCRxH CCR1_MSIZE msizeH

  let (prioH,prioL) = case _Priority config of
        Low        -> (False,False)
        Medium     -> (False, True)
        High       -> (True ,False)
        VeryHigh   -> (True ,True )
  writeCCRxL CCR1_PL prioL
  writeCCRxH CCR1_PL prioH

  writeCCRx CCR1_MEM2MEM $ case _Direction config of
     Mem2Mem       -> True
     PeripheralSRC -> False
     PeripheralDST -> False

  poke cndtr $ fromIntegral $ _BufferSize config
  poke cpar  $ _PeripheralBaseAddr config
  poke cmar  $ _MemoryBaseAddr config

cmd :: Channel -> Bool -> MI ()
cmd c rs
  = writeCCRxOffset 0 c CCR1_EN rs

enable :: Channel -> MI ()
enable c = cmd c True

disable :: Channel -> MI ()
disable c = cmd c False

deInit :: Channel -> MI()
deInit channel = do
  let  poke  = pokeReg $ channelToPeripheral channel

  disable channel
  poke (channelToCCR channel) 0
  poke (channelToCNDTR channel) 0
  poke (channelToCPAR channel) 0
  poke (channelToCMAR channel) 0
