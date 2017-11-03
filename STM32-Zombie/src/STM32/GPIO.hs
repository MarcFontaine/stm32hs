----------------------------------------------------------------------------
-- |
-- Module      :  STM32.GPIO
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- General Purpose Input Output
{-# LANGUAGE OverloadedStrings #-}
module STM32.GPIO
where

import Device
import STM32.MachineInterface
import STM32.Utils

data Pin
  = Pin_0 | Pin_1 | Pin_2 | Pin_3 | Pin_4 | Pin_5 | Pin_6 | Pin_7 | Pin_8
  | Pin_9 | Pin_10 | Pin_11 | Pin_12 | Pin_13 | Pin_14 | Pin_15
  deriving (Show,Ord,Eq,Enum)

type Wire = (Peripheral,Pin)
  
pinOut :: Wire -> Bool -> MI ()
pinOut (p,pin) rs = case rs of
   True  -> bitSet p $ bsFromPin pin
   False -> bitSet p $ brFromPin pin

pinHigh :: Wire -> MI ()
pinHigh w = pinOut w True

pinLow :: Wire -> MI ()
pinLow w = pinOut w False

data Speed
  = MHz_10
  | MHz_2
  | MHz_50
  deriving (Eq,Ord,Show)

instance ToBitField Speed where
  toBitField s = case s of
    MHz_10 -> "01"
    MHz_2  -> "10"
    MHz_50 -> "11"
        
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
pinMode (p,n) m = do
  regFieldWrite p (cnfFromPin n) $ case m of
    GPOutPushPull         _ -> "00"
    GPOutOpenDrain        _ -> "01"
    AlternateOutPushPull  _ -> "10"
    AlternateOutOpenDrain _ -> "11"
    InputAnalog             -> "00"
    InputFloating           -> "01"
    InputPullDown           -> "10"
    InputPullUp             -> ("10" :: BitField)

  regFieldWrite p (modeFromPin n) $ case m of
    GPOutPushPull         s -> toBitField s
    GPOutOpenDrain        s -> toBitField s
    AlternateOutPushPull  s -> toBitField s
    AlternateOutOpenDrain s -> toBitField s
    InputAnalog             -> "00"
    InputFloating           -> "00"
    InputPullDown           -> "00" 
    InputPullUp             -> "00" 
  case m of
    InputPullDown -> pinLow (p,n)
    InputPullUp   -> pinHigh (p,n)
    _ -> return ()
  
cnfFromPin :: Pin -> Field
cnfFromPin p = cnf
  where
    (cnf,_,_,_) = pinToFields p

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

pinToFields :: Pin -> (Field,Field,Field,Field)
pinToFields p = case p of
  Pin_0 ->  ( CRL_CNF0 ,  CRL_MODE0 , BSRR_BS0 , BSRR_BR0  ) 
  Pin_1 ->  ( CRL_CNF1 ,  CRL_MODE1 , BSRR_BS1 , BSRR_BR1  )
  Pin_2 ->  ( CRL_CNF2 ,  CRL_MODE2 , BSRR_BS2 , BSRR_BR2  )
  Pin_3 ->  ( CRL_CNF3 ,  CRL_MODE3 , BSRR_BS3 , BSRR_BR3  )
  Pin_4 ->  ( CRL_CNF4 ,  CRL_MODE4 , BSRR_BS4 , BSRR_BR4  )
  Pin_5 ->  ( CRL_CNF5 ,  CRL_MODE5 , BSRR_BS5 , BSRR_BR5  )
  Pin_6 ->  ( CRL_CNF6 ,  CRL_MODE6 , BSRR_BS6 , BSRR_BR6  )
  Pin_7 ->  ( CRL_CNF7 ,  CRL_MODE7 , BSRR_BS7 , BSRR_BR7  )
  Pin_8  -> ( CRH_CNF8 ,  CRH_MODE8 , BSRR_BS8 , BSRR_BR8  )
  Pin_9  -> ( CRH_CNF9 ,  CRH_MODE9 , BSRR_BS9 , BSRR_BR9  )
  Pin_10 -> ( CRH_CNF10 , CRH_MODE10 ,BSRR_BS10 ,BSRR_BR10 )
  Pin_11 -> ( CRH_CNF11 , CRH_MODE11 ,BSRR_BS11 ,BSRR_BR11 )
  Pin_12 -> ( CRH_CNF12 , CRH_MODE12 ,BSRR_BS12 ,BSRR_BR12 )
  Pin_13 -> ( CRH_CNF13 , CRH_MODE13 ,BSRR_BS13 ,BSRR_BR13 )
  Pin_14 -> ( CRH_CNF14 , CRH_MODE14 ,BSRR_BS14 ,BSRR_BR14 )
  Pin_15 -> ( CRH_CNF15 , CRH_MODE15 ,BSRR_BS15 ,BSRR_BR15 )
