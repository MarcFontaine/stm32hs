----------------------------------------------------------------------------
-- |
-- Module      :  F401.GPIO
-- Copyright   :  (c) Marc Fontaine 2017-2025
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- General Purpose Input Output
{-# LANGUAGE OverloadedStrings #-}
module F401.GPIO
where

import           Device
import           F401.MachineInterface
import           F401.Utils

data Pin
  = Pin_0 | Pin_1 | Pin_2 | Pin_3 | Pin_4 | Pin_5 | Pin_6 | Pin_7 | Pin_8
  | Pin_9 | Pin_10 | Pin_11 | Pin_12 | Pin_13 | Pin_14 | Pin_15
  deriving (Show,Ord,Eq,Enum)

type Wire = (Peripheral,Pin)

pinOut :: Wire -> Bool -> MI ()
pinOut (p,pin) rs = if rs
  then bitSet p $ bsFromPin pin
  else bitSet p $ brFromPin pin

pinHigh :: Wire -> MI ()
pinHigh w = pinOut w True

pinLow :: Wire -> MI ()
pinLow w = pinOut w False

data Speed
  = LowSpeed
  | MediumSpeed
  | HighSpeed
  | VeryHighSpeed
  deriving (Eq,Ord,Show)

instance ToBitField Speed where
  toBitField s = case s of
    LowSpeed      -> "00"
    MediumSpeed   -> "01"
    HighSpeed     -> "10"
    VeryHighSpeed -> "11"

data PinMode
  = GPOutPushPull Speed
  | GPOutOpenDrain Speed
  | AlternateOutPushPull Speed
  | AlternateOutOpenDrain Speed
  | InputAnalog
  | InputFloating
  | InputPullDown
  | InputPullUp
  deriving (Eq,Ord,Show)

pinMode :: Wire -> PinMode -> MI ()
pinMode (p, n) m = do
  regFieldWrite p (modeFromPin n) $ case m of
    GPOutPushPull         _ -> "01"
    GPOutOpenDrain        _ -> "01"
    AlternateOutPushPull  _ -> "10"
    AlternateOutOpenDrain _ -> "10"
    InputAnalog             -> "11"
    InputFloating           -> "00"
    InputPullDown           -> "00"
    InputPullUp             -> ("00" :: BitField)

  regFieldWrite p (speedFromPin n) $ case m of
    GPOutPushPull         s -> toBitField s
    GPOutOpenDrain        s -> toBitField s
    AlternateOutPushPull  s -> toBitField s
    AlternateOutOpenDrain s -> toBitField s
    InputAnalog             -> "00"
    InputFloating           -> "00"
    InputPullDown           -> "00"
    InputPullUp             -> "00"
  case m of
    InputPullDown -> pinLow (p, n)
    InputPullUp   -> pinHigh (p, n)
    _             -> return ()

speedFromPin :: Pin -> Field
speedFromPin p = sp
  where
    (sp, _, _, _) = pinToFields p

modeFromPin :: Pin -> Field
modeFromPin p = m
  where
    (_,m,_,_) = pinToFields p

bsFromPin :: Pin -> Field
bsFromPin p = bs
  where
    (_,_,bs,_) = pinToFields p

brFromPin :: Pin -> Field
brFromPin p = br
  where
    (_,_,_,br) = pinToFields p

pinToFields :: Pin -> (Field, Field, Field, Field)
pinToFields p = case p of
  Pin_0  ->  ( OSPEEDR_OSPEEDR0,  MODER_MODER0,  BSRR_BS0,  BSRR_BR0  )
  Pin_1  ->  ( OSPEEDR_OSPEEDR1,  MODER_MODER1,  BSRR_BS1,  BSRR_BR1  )
  Pin_2  ->  ( OSPEEDR_OSPEEDR2,  MODER_MODER2,  BSRR_BS2,  BSRR_BR2  )
  Pin_3  ->  ( OSPEEDR_OSPEEDR3,  MODER_MODER3,  BSRR_BS3,  BSRR_BR3  )
  Pin_4  ->  ( OSPEEDR_OSPEEDR4,  MODER_MODER4,  BSRR_BS4,  BSRR_BR4  )
  Pin_5  ->  ( OSPEEDR_OSPEEDR5,  MODER_MODER5,  BSRR_BS5,  BSRR_BR5  )
  Pin_6  ->  ( OSPEEDR_OSPEEDR6,  MODER_MODER6,  BSRR_BS6,  BSRR_BR6  )
  Pin_7  ->  ( OSPEEDR_OSPEEDR7,  MODER_MODER7,  BSRR_BS7,  BSRR_BR7  )
  Pin_8  -> ( OSPEEDR_OSPEEDR8,  MODER_MODER8,  BSRR_BS8,  BSRR_BR8  )
  Pin_9  -> ( OSPEEDR_OSPEEDR9,  MODER_MODER9,  BSRR_BS9,  BSRR_BR9  )
  Pin_10 -> ( OSPEEDR_OSPEEDR10, MODER_MODER10, BSRR_BS10, BSRR_BR10 )
  Pin_11 -> ( OSPEEDR_OSPEEDR11, MODER_MODER11, BSRR_BS11, BSRR_BR11 )
  Pin_12 -> ( OSPEEDR_OSPEEDR12, MODER_MODER12, BSRR_BS12, BSRR_BR12 )
  Pin_13 -> ( OSPEEDR_OSPEEDR13, MODER_MODER13, BSRR_BS13, BSRR_BR13 )
  Pin_14 -> ( OSPEEDR_OSPEEDR14, MODER_MODER14, BSRR_BS14, BSRR_BR14 )
  Pin_15 -> ( OSPEEDR_OSPEEDR15, MODER_MODER15, BSRR_BS15, BSRR_BR15 )
