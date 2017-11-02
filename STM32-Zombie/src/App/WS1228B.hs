----------------------------------------------------------------------------
-- |
-- Module      :  App.WS1228B
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The WS1228Bs are popular RGB LED controllers for colorful decorations and
-- mood lights etc.
-- For proper operation the WS1228B requires fast and acurate timing.
-- The example works with combination of SPI and DMA.
-- With the SPI port it is possible to shift out a raw bitstream.
-- (i.e. play a one-bit sampled wave-form).
-- (This is not possible with the USART because the USART would add start and stop bits)

module App.WS1228B
where

import STM32.API as API
import STM32.GPIO as GPIO
import STM32.SPI as SPI
import STM32.DMA as DMA

import qualified Data.ByteString as BS
import Control.Monad

data RGB = RGB Word8 Word8 Word8
  deriving (Read,Show,Eq,Ord)

-- | show some color pattern
testLEDs :: IO ()
testLEDs = sendLEDs [red,green,blue,black,white]

-- | turn off the first 30 LEDs (== set the color to black black)
ledsOff30 :: IO ()
ledsOff30 = sendLEDs $ replicate 30 black

-- | set the LEDs to a list of colors.
sendLEDs :: [RGB] -> IO ()
sendLEDs colors = runMI $ do
  initSPI
  sendSPI $ encodeRGBLine colors

black :: RGB
black  = RGB 0x00 0x00 0x00

white :: RGB
white  = RGB 0xff 0xff 0xff

red   :: RGB
red    = RGB 0xff 0x00 0x00

green :: RGB
green  = RGB 0x00 0xff 0x00

blue  :: RGB
blue  =  RGB 0x00 0x00 0xff

-- | The WS1228B protocoll.
-- translate a list of colors to the transmission bits.
encodeRGBLine :: [RGB] -> BS.ByteString
encodeRGBLine l = BS.concat (resetCode : map encodeRGB l)

resetCode :: BS.ByteString
resetCode = BS.pack $ replicate 20 0x00

encodeRGB :: RGB -> BS.ByteString
encodeRGB (RGB r g b)
  = BS.pack [g3,g2,g1,r3,r2,r1,b3,b2,b1]
  where
    (r3,r2,r1) = lineCodeWord8 r
    (g3,g2,g1) = lineCodeWord8 g
    (b3,b2,b1) = lineCodeWord8 b

-- | Encode an Word8 according to the WS1228B line code.
-- each bit get extended to three bits
lineCodeWord8 :: Word8 -> (Word8,Word8,Word8)
lineCodeWord8 b = (c1,c2,c3)
  where
    c1 = fromIntegral ((mix32 `shiftR` 16) .&. 0xff)
    c2 = fromIntegral ((mix32 `shiftR` 8) .&. 0xff)
    c3 = fromIntegral (mix32 .&. 0xff)
    mix32 :: Word32
    mix32 = worker 7 0
    worker (-1) accum = accum
    worker n accum = worker (n -1) ((accum `shiftL` 3) .|. bitCode)
      where bitCode = if b `testBit` n then 6 else 4


{-
spi_nss :: Wire
spi_nss =(GPIOB,Pin_12)
spi_sck :: Wire
spi_sck =(GPIOB,Pin_13)
spi_miso :: Wire       
spi_miso=(GPIOB,Pin_14)
-}
led :: Wire
--led = (GPIOC,Pin_13)
led = (GPIOA,Pin_12)

spi_mosi :: Wire       
spi_mosi=(GPIOB,Pin_15)

spiConfig :: SPI.Config
spiConfig = SPI.Config {
    _direction   = One_Line_Tx
  , _mode        = Master
  , _dataSize    = Eight
  , _CPOL        = SPI.Low
  , _CPHA        = OneEdge
  , _NSS         = Soft
  , _baudRatePrescaler = Prescaler_16
  , _firstBit          = MSB
  , _CRCPolynomial     = 7
  }


initSPI :: MI ()
initSPI = do
  initMI
  API.resetHalt  
  setDefaultClocks
  SPI.deInit SPI2
  peripheralClockOn GPIOB
  peripheralClockOn GPIOC
  peripheralClockOn SPI2
  pinMode led $ GPOutPushPull Mhz_2
  pinMode spi_mosi $ GPIO.AlternateOutPushPull Mhz_2
  SPI.init SPI2 spiConfig
  bitSet SPI2 CR2_TXDMAEN

  SPI.enable SPI2

sendSPI :: BS.ByteString -> MI ()
sendSPI bs = do
  let len = BS.length bs
      dmaBuffer = 0x20001000 
      dmaConfig = DMA.Config {
        _BufferSize         = fromIntegral $ len
       ,_Direction          = PeripheralDST
       ,_MemoryBaseAddr     = dmaBuffer
       ,_MemoryDataSize     = Byte
       ,_MemoryInc          = True
       ,DMA._Mode           = Normal
       ,_PeripheralBaseAddr = regToAddr SPI2 DR
       ,_PeripheralDataSize = Byte
       ,_PeripheralInc      = False
       ,_Priority           = DMA.High                              
      }
  writeMem8 dmaBuffer bs

  peripheralClockOn DMA1
  DMA.deInit DMA1_Channel5
  
  DMA.disable DMA1_Channel5
  DMA.init DMA1_Channel5 dmaConfig
  DMA.enable DMA1_Channel5

  return ()

-- | Animate LEDs and show some wave like lighting pattern
testWave :: IO ()
testWave = runMI $ do
  initSPI
  let
    st = 2*pi/10
    loop t = do
     let colors = [RGB (redIntensity $ wave t st i)
                       (redIntensity $ wave (-t*0.5) st i) 0
                  | i <- [0..15]] 
     sendSPI $ encodeRGBLine colors
     delay 1000
     loop $ t + 0.1
  loop 0

wave :: Double -> Double -> Int -> Double
wave t st i = (sin (t+st* fromIntegral i) +1) /2

redIntensity :: Double -> Word8
redIntensity d =
  if d >0.4 then floor (d*5)
  else 0
