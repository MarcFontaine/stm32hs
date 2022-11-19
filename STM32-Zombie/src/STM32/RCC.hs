----------------------------------------------------------------------------
-- |
-- Module      :  STM32.RCC
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Clock control and
-- resetting parts of the hardware.

{-# LANGUAGE OverloadedStrings #-}
module STM32.RCC
where

import Device
import STM32.MachineInterface
import STM32.Utils

deInit :: MI ()
deInit = do
  bitSet RCC CR_HSION
  andReg RCC CFGR 0xF8FF0000

  bitReset RCC CR_HSEON
  bitReset RCC CR_CSSON
  bitReset RCC CR_PLLON

  bitReset RCC CR_HSEBYP

  andReg RCC CFGR 0xFF80FFFF
  pokeReg RCC CIR 0

set_HSE_OFF :: MI ()
set_HSE_OFF = do
  bitReset RCC CR_HSEON
  bitReset RCC CR_HSEBYP

set_HSE_ON  :: MI ()
set_HSE_ON = do
  set_HSE_OFF
  bitSet RCC CR_HSEON

set_HSE_Bypass  :: MI ()
set_HSE_Bypass = do
  set_HSE_ON
  bitSet RCC CR_HSEBYP


peripheralClockOn :: Peripheral -> MI ()
peripheralClockOn  = peripheralClock True

peripheralClockOff :: Peripheral -> MI ()
peripheralClockOff  = peripheralClock False

peripheralClock :: Bool -> Peripheral -> MI ()
peripheralClock rs p = bitWrite RCC (peripheralClockField p) rs

peripheralClockField :: Peripheral -> Field
peripheralClockField p = case p of
  I2C1   -> APB1ENR_I2C1EN
  SPI3   -> APB1ENR_SPI3EN
  TIM2   -> APB1ENR_TIM2EN
  TIM6   -> APB1ENR_TIM6EN
  USART2 -> APB1ENR_USART2EN
  BKP    -> APB1ENR_BKPEN
  I2C2   -> APB1ENR_I2C2EN
  TIM12  -> APB1ENR_TIM12EN
  TIM3   -> APB1ENR_TIM3EN
  TIM7   -> APB1ENR_TIM7EN
  USART3 -> APB1ENR_USART3EN
  CAN    -> APB1ENR_CANEN
  PWR    -> APB1ENR_PWREN
  TIM13  -> APB1ENR_TIM13EN
  TIM4   -> APB1ENR_TIM4EN
  UART4  -> APB1ENR_UART4EN
  USB    -> APB1ENR_USBEN
  DAC    -> APB1ENR_DACEN
  SPI2   -> APB1ENR_SPI2EN
  TIM14  -> APB1ENR_TIM14EN
  TIM5   -> APB1ENR_TIM5EN
  UART5  -> APB1ENR_UART5EN
  WWDG   -> APB1ENR_WWDGEN

  GPIOA  -> APB2ENR_IOPAEN
  GPIOB  -> APB2ENR_IOPBEN
  GPIOC  -> APB2ENR_IOPCEN
  GPIOD  -> APB2ENR_IOPDEN
  GPIOE  -> APB2ENR_IOPEEN
  GPIOF  -> APB2ENR_IOPFEN
  GPIOG  -> APB2ENR_IOPGEN
  ADC1   -> APB2ENR_ADC1EN
  ADC2   -> APB2ENR_ADC2EN
  ADC3   -> APB2ENR_ADC3EN
  SPI1   -> APB2ENR_SPI1EN
  TIM1   -> APB2ENR_TIM1EN
  TIM8   -> APB2ENR_TIM8EN
  TIM9   -> APB2ENR_TIM9EN
  TIM10  -> APB2ENR_TIM10EN
  TIM11  -> APB2ENR_TIM11EN
  USART1 -> APB2ENR_USART1EN
  AFIO   -> APB2ENR_AFIOEN
  DMA1   -> AHBENR_DMA1EN
  DMA2   -> AHBENR_DMA2EN
  SDIO   -> AHBENR_SDIOEN
  CRC    -> AHBENR_CRCEN
  FSMC   -> AHBENR_FSMCEN
  -- TODO:
  NVIC   -> error "NVIC TODO clock"
  RCC    -> error "RCC TODO clock"
  RTC    -> error "RTC TODO clock"
  DBG    -> error "DBG TODO clock"
  EXTI   -> error "EXTI TODO clock"
  FLASH  -> error "FLASH TODO clock"
  IWDG   -> error "IWDG TODO clock"

peripheralReset :: Bool -> Peripheral -> MI ()
peripheralReset rs p = bitWrite RCC  (peripheralResetField p) rs

peripheralResetToggle :: Peripheral -> MI ()
peripheralResetToggle p = do
  peripheralReset True p
  peripheralReset False p

peripheralResetField :: Peripheral -> Field
peripheralResetField p = case p of
  AFIO   -> APB2RSTR_AFIORST
  GPIOD -> APB2RSTR_IOPDRST
  SPI1  -> APB2RSTR_SPI1RST
  TIM8  -> APB2RSTR_TIM8RST
  ADC1  -> APB2RSTR_ADC1RST
  GPIOA -> APB2RSTR_IOPARST
  GPIOE -> APB2RSTR_IOPERST
  TIM10 -> APB2RSTR_TIM10RST
  TIM9  -> APB2RSTR_TIM9RST
  ADC2  -> APB2RSTR_ADC2RST
  GPIOB -> APB2RSTR_IOPBRST
  GPIOF -> APB2RSTR_IOPFRST
  TIM11 -> APB2RSTR_TIM11RST
  USART1 -> APB2RSTR_USART1RST
  ADC3   -> APB2RSTR_ADC3RST
  GPIOC  -> APB2RSTR_IOPCRST
  GPIOG  -> APB2RSTR_IOPGRST
  TIM1   -> APB2RSTR_TIM1RST
  BKP    -> APB1RSTR_BKPRST
  I2C2   -> APB1RSTR_I2C2RST
  TIM12  -> APB1RSTR_TIM12RST
  TIM3   -> APB1RSTR_TIM3RST
  TIM7   -> APB1RSTR_TIM7RST
  USART3 -> APB1RSTR_USART3RST
  CAN    -> APB1RSTR_CANRST
  PWR    -> APB1RSTR_PWRRST
  TIM13  -> APB1RSTR_TIM13RST
  TIM4   -> APB1RSTR_TIM4RST
  UART4  -> APB1RSTR_UART4RST
  USB    -> APB1RSTR_USBRST
  DAC    -> APB1RSTR_DACRST
  SPI2   -> APB1RSTR_SPI2RST
  TIM14  -> APB1RSTR_TIM14RST
  TIM5   -> APB1RSTR_TIM5RST
  UART5  -> APB1RSTR_UART5RST
  WWDG   -> APB1RSTR_WWDGRST
  I2C1   -> APB1RSTR_I2C1RST
  SPI3   -> APB1RSTR_SPI3RST
  TIM2   -> APB1RSTR_TIM2RST
  TIM6   -> APB1RSTR_TIM6RST
  USART2 -> APB1RSTR_USART2RST
  -- TODO:
  CRC    -> error "CRC TODO reset"
  DBG    -> error "DBG TODO reset"
  DMA1   -> error "DMA1 TODO reset"
  DMA2   -> error "DMA2 TODO reset"
  FSMC   -> error "FSMC TODO reset"
  SDIO   -> error "SDIO TODO reset"
  NVIC   -> error "NVIC TODO reset"
  RCC    -> error "RCC TODO reset"
  RTC    -> error "RTC TODO reset"
  EXTI   -> error "EXTI TODO reset"
  FLASH  -> error "FLASH TODO reset"
  IWDG   -> error "IWDG TODO reset"

data SYSCLK_Div
  = SYSCLK_Div1 | SYSCLK_Div2 | SYSCLK_Div4 | SYSCLK_Div8
  | SYSCLK_Div16 | SYSCLK_Div64 | SYSCLK_Div128
  | SYSCLK_Div256 | SYSCLK_Div512 deriving (Show,Eq)

instance RegisterField SYSCLK_Div where
  toBits d = case d of
    SYSCLK_Div1   -> "0000"
    SYSCLK_Div2   -> "1000"
    SYSCLK_Div4   -> "1001"
    SYSCLK_Div8   -> "1010"
    SYSCLK_Div16  -> "1011"
    SYSCLK_Div64  -> "1100"
    SYSCLK_Div128 -> "1101"
    SYSCLK_Div256 -> "1110"
    SYSCLK_Div512 -> "1111"
  toField = const CFGR_HPRE

hCLKConfig :: SYSCLK_Div -> MI()
hCLKConfig = fieldWrite RCC

data HCLK_Div
  = HCLK_Div1 | HCLK_Div2 | HCLK_Div4 | HCLK_Div8 | HCLK_Div16
  deriving Show

instance ToBitField HCLK_Div where
  toBitField d = case d of
    HCLK_Div1  -> "000"
    HCLK_Div2  -> "100"
    HCLK_Div4  -> "101"
    HCLK_Div8  -> "110"
    HCLK_Div16 -> "111"


pCLK1Config :: HCLK_Div -> MI()
pCLK1Config = regFieldWrite RCC CFGR_PPRE1

pCLK2Config :: HCLK_Div -> MI()
pCLK2Config = regFieldWrite RCC CFGR_PPRE2

data PLLSource
  = PLLSource_HSI_Div2 | PLLSource_HSE_Div1 | PLLSource_HSE_Div2
  deriving Show

data PLLMul
  = PLLMul4 | PLLMul5 | PLLMul6 | PLLMul7 | PLLMul8 | PLLMul9 | PLLMul65
  deriving Show

instance RegisterField PLLMul where
  toBits m = case m of
    PLLMul4  -> "0010"
    PLLMul5  -> "0011"
    PLLMul6  -> "0100"
    PLLMul7  -> "0101"
    PLLMul8  -> "0110"
    PLLMul9  -> "0111"
    PLLMul65 -> "1101"
  toField = const CFGR_PLLMUL

pllConfig :: PLLSource -> PLLMul -> MI ()
pllConfig source mult = do
  let set f = bitWrite RCC f
  set CFGR_PLLXTPRE $ case source of
    PLLSource_HSE_Div1 -> False
    PLLSource_HSE_Div2 -> True
    PLLSource_HSI_Div2 -> True

  set CFGR_PLLSRC $ case source of
    PLLSource_HSI_Div2 -> False
    PLLSource_HSE_Div1 -> True
    PLLSource_HSE_Div2 -> True

  fieldWrite RCC mult

data SYSCLKSource
  = SYSCLKSource_HSI
  | SYSCLKSource_HSE
  | SYSCLKSource_PLLCLK

instance RegisterField SYSCLKSource where
  toBits s = case s of
    SYSCLKSource_HSI     -> "00"
    SYSCLKSource_HSE     -> "01"
    SYSCLKSource_PLLCLK  -> "10"
  toField = const CFGR_SW

sysCLKConfig :: SYSCLKSource -> MI()
sysCLKConfig = fieldWrite RCC

pllCmd :: Bool -> MI ()
pllCmd rs = bitWrite RCC CR_PLLON rs

pllCmdEnable :: MI ()
pllCmdEnable = pllCmd True

setDefaultClocks :: MI()
setDefaultClocks = do
  deInit
  set_HSE_ON

  hCLKConfig SYSCLK_Div1
  pCLK2Config HCLK_Div1
  pCLK1Config HCLK_Div2

  pllConfig PLLSource_HSE_Div1 PLLMul9
  pllCmdEnable
  sysCLKConfig SYSCLKSource_PLLCLK


data LSE = LSE_OFF | LSE_ON | LSE_Bypass
  deriving (Show,Eq)

lseConfig :: LSE -> MI ()
lseConfig lse = do
  andReg RCC BDCR 0xffffff00 -- stmlib uses u8-access here ?
  case lse of
    LSE_OFF    -> return ()
    LSE_ON     -> bitSet RCC BDCR_LSEON
    LSE_Bypass -> bitSet RCC BDCR_LSEON >> bitSet RCC BDCR_LSEBYP

data RtcClockSource = LSE | LSI | HSE_Div128
  deriving (Show,Eq)

instance RegisterField RtcClockSource where
  toBits d = case d of
    LSE -> "01"
    LSI -> "10"
    HSE_Div128 -> "11"
  toField = const BDCR_RTCSEL

rtcClockConfig :: RtcClockSource -> MI ()
rtcClockConfig = fieldWrite RCC

rtcClkCmd :: Bool -> MI ()
rtcClkCmd = bitWrite RCC BDCR_RTCEN
