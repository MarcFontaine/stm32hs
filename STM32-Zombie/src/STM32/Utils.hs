----------------------------------------------------------------------------
-- |
-- Module      :  STM32.Utils
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Utility functions for hardware register access.

{-# LANGUAGE FlexibleInstances #-}
module STM32.Utils
where
import STM32.MachineInterface
import Data.Word
import Data.Bits
import Data.String
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (threadDelay)
import Device       

delay :: Int -> MI ()
delay = liftIO . threadDelay
       
regToAddr :: Peripheral -> Register -> Word32
regToAddr p r = peripheralBase p + registerOffset p r

fieldToAddr :: Peripheral -> Field -> Word32
fieldToAddr p f = regToAddr p $ fieldToRegister f

peekReg :: Peripheral -> Register -> MI Word32
peekReg p r = peek_w32 $ regToAddr p r

-- ? do we have any 32 bit registers?
pokeReg :: Peripheral -> Register -> Word32 -> MI ()
pokeReg p r = poke_w32 $ regToAddr p r

andReg :: Peripheral -> Register -> Word32 -> MI ()
andReg p r w = do
   tmp <- peekReg p r
   pokeReg p r $ tmp .&. w

orReg :: Peripheral -> Register -> Word32 -> MI ()
orReg p r w = do
   tmp <- peekReg p r
   pokeReg p r $ tmp .|. w


peekLHReg :: Peripheral -> (Register,Register) -> MI Word32
peekLHReg p (l,h)
  = fromLH <$> peekReg p l <*> peekReg p h

pokeLHReg :: Peripheral -> (Register,Register) -> Word32 -> MI ()
pokeLHReg p (l,h) val = do
  pokeReg p l (val .&. 0xffff)
  pokeReg p h (val `shiftR` 16)


fromLH :: Word32 -> Word32 -> Word32
fromLH l h = (h `shiftL` 16) .|. (l .&. 0xffff) 
  
print':: Show x => x -> MI ()
print' = liftIO . print

bitSet :: Peripheral -> Field -> MI ()
bitSet p f = bitWrite p f True

bitReset :: Peripheral -> Field -> MI ()
bitReset p f = bitWrite p f False

class ToBit a where
  toBit :: a -> Bool

instance ToBit Bool where toBit = id
         
bitWrite :: ToBit b => Peripheral -> Field -> b -> MI ()
bitWrite p f rs = do
  when (fieldBitWidth f /= 1) $ error "bitSet: fieldWidth not 1"
  bitWriteRaw rs
    (fieldToAddr p f)
    (fieldBitOffset f)

class RegisterField f where
  toBits :: f -> BitField
  toField :: f -> Field
  
class ToBitField f where
  toBitField :: f -> BitField
  
instance ToBitField [Bool] where toBitField = BitField
instance ToBitField BitField where toBitField = id
newtype BitField = BitField {unBitField :: [Bool]}

instance IsString BitField
  where fromString = BitField . toBList

toBList :: String -> [Bool]
toBList = reverse . map toB
  where
    toB '0' = False
    toB '1' = True
    toB _ = error "toBList: no binary"  
                  
fieldWrite :: RegisterField f => Peripheral -> f -> MI ()
fieldWrite p regField
  = regFieldWrite p (toField regField) (toBits regField)

regFieldWrite :: ToBitField f => Peripheral -> Field -> f -> MI ()
regFieldWrite p f bits' = do
  let bits= unBitField $ toBitField bits'
  when (fieldBitWidth f /=  length bits)
     $ error "fieldWrite: fieldWidth does not match argument"
  fieldWriteRaw
    (fieldToAddr p f) 
    (enumFrom $ fieldBitOffset f)
    bits
  
fieldWriteRaw :: Word32 -> [Int] -> [Bool] -> MI ()
fieldWriteRaw addr offsets bits              
  = zipWithM_ (\o b -> bitWriteRaw b addr o) offsets bits 

bitWriteRaw :: ToBit b => b -> Word32 -> Int -> MI ()
bitWriteRaw rs addr bitNum = do
  bbAddr <- case toBidBand addr bitNum of
                Just r -> return r
                Nothing -> error "todo: bitWrite implement none bitband"
  case toBit rs of
    True -> poke_w32 bbAddr 1
    False -> poke_w32 bbAddr 0

bitWrite_alt :: Bool -> Peripheral -> Field -> MI ()
bitWrite_alt rs p f = do
  let
    r = fieldToRegister f
    bitNum = fieldBitOffset f 
  old <- peekReg p r
  let new = case rs of
          True -> old .|. (1 `shiftL` bitNum)
          False -> old .&.(0xfffffffe `shiftL` bitNum)
  pokeReg p r new

  
toBidBand :: Word32 -> Int -> Maybe Word32
toBidBand addr bitNum = case addr of
    _ | 0x20000000 <= addr && addr <= 0x200FFFFF
        -> Just $ (bit_word_offset $ addr - 0x20000000) + 0x22000000
    _ | 0x40000000 <= addr && addr <= 0x400FFFFF
        -> Just $ (bit_word_offset $ addr - 0x40000000) + 0x42000000
    _ -> Nothing
   where
     bit_word_offset byte = byte*32 + (fromIntegral bitNum) * 4
