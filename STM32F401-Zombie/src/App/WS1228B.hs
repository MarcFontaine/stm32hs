----------------------------------------------------------------------------
-- |
-- Module      :  App.WS1228B
-- Copyright   :  (c) Marc Fontaine 2017-2024
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The popular WS1228B module consists of a RGB LED and an included LED controller.
-- Many WS1228B modules can be chained up to build LED strips
-- for colorful decorations, mood lights etc.
-- For proper operation the WS1228B requires fast and accurate timing.
-- The example works with combination of SPI and DMA.
-- With the SPI port it is possible to shift out a raw bit-stream.
-- (i.e. play a one-bit sampled wave-form).
-- (This is not possible with the USART because the USART would add start and stop bits)

module App.WS1228B
where

import STM32.API as API
import STM32.GPIO as GPIO
import STM32.SPI as SPI
import STM32.DMA as DMA

import qualified Data.ByteString as BS
import Data.List (tails)
import qualified Data.Array.Unboxed as Array
import Data.Array.Unboxed (UArray)

data RGB = RGB Word8 Word8 Word8
  deriving (Read, Show, Eq, Ord)

-- | show some color pattern
testLEDs :: IO ()
testLEDs = sendLEDs [red,green,blue,black,white]

-- | turn off the first 30 LEDs (== set the color to black)
ledsOff30 :: IO ()
ledsOff30 = sendLEDs $ replicate 30 black

-- | set the LED strip to a list of colors.
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
-- Each data bit is extended to a three bit line code.
lineCodeWord8 :: Word8 -> (Word8, Word8, Word8)
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
led = (GPIOA, Pin_12)

spi_mosi :: Wire
spi_mosi = (GPIOB, Pin_15)

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
  pinMode led $ GPOutPushPull MHz_2
  pinMode spi_mosi $ GPIO.AlternateOutPushPull MHz_2
  SPI.init SPI2 spiConfig
  bitSet SPI2 CR2_TXDMAEN

  SPI.enable SPI2

sendSPI :: BS.ByteString -> MI ()
sendSPI bs = do
  let len = BS.length bs
      dmaBuffer = 0x20001000
      dmaConfig = DMA.Config {
        _BufferSize         = fromIntegral len
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

-- | Animate a LED strip and show some wave-like lightning pattern.
testWave :: IO ()
testWave = runMI $ do
  initSPI
  let
    st = 2*pi/60
    loop t = do
     let colors = [ gammaCorrect
                     (wave t st i)
                     (wave (t*0.5) st i)
                     (wave (t*0.1) st i)
                  | i <- [0..59]]
     sendSPI $ encodeRGBLine colors
     delay 500
     loop $ t + 0.23
  loop 0

wave :: Double -> Double -> Int -> Double
wave t st i = (sin (t+st* fromIntegral i) +1) /2

gammaCorrect :: Double -> Double -> Double -> RGB
gammaCorrect r g b = RGB rb gb bb
  where
    rb = gammaTable Array.! (floor $ r * 256)
    gb = gammaTable Array.! (floor $ g * 256)
    bb = gammaTable Array.! (floor $ b * 256)

-- http://rgb-123.com/ws2812-color-output/
gammaTable :: UArray Word8 Word8
gammaTable = Array.listArray (0,255) gammaList
  where
    gammaList :: [Word8]
    gammaList = [
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2,
      2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5,
      6, 6, 6, 7, 7, 7, 8, 8, 8, 9, 9, 9, 10, 10, 11, 11,
      11, 12, 12, 13, 13, 13, 14, 14, 15, 15, 16, 16, 17, 17, 18, 18,
      19, 19, 20, 21, 21, 22, 22, 23, 23, 24, 25, 25, 26, 27, 27, 28,
      29, 29, 30, 31, 31, 32, 33, 34, 34, 35, 36, 37, 37, 38, 39, 40,
      40, 41, 42, 43, 44, 45, 46, 46, 47, 48, 49, 50, 51, 52, 53, 54,
      55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
      71, 72, 73, 74, 76, 77, 78, 79, 80, 81, 83, 84, 85, 86, 88, 89,
      90, 91, 93, 94, 95, 96, 98, 99,100,102,103,104,106,107,109,110,
      111,113,114,116,117,119,120,121,123,124,126,128,129,131,132,134,
      135,137,138,140,142,143,145,146,148,150,151,153,155,157,158,160,
      162,163,165,167,169,170,172,174,176,178,179,181,183,185,187,189,
      191,193,194,196,198,200,202,204,206,208,210,212,214,216,218,220,
      222,224,227,229,231,233,235,237,239,241,244,246,248,250,252,255 ]

-- this leaks memory !!
testPattern :: IO ()
testPattern = runMI $ loop config
  where
    loop :: [([RGB], Int, Bool)] -> MI ()
    loop [] = return ();
    loop ((p, d, dir) : next) = do
      let
          p2 = (\x -> if dir then x else reverse x) $ take 15 p
          pe = reverse p2 ++ p2
      sendSPI $ encodeRGBLine pe
      delay d
      loop next

    config :: [([RGB], Int, Bool)]
    config = zip3 (tails pat) speed rev

    pat = cycle [red, red, red, green, green, blue, blue, blue, black, black, black ,black, black]
    speed = cycle ((reverse ramp ++ replicate 300 acc ++ ramp) ++ reverse (reverse ramp ++ replicate 300 acc ++ ramp))
    ramp  = [acc, acc+acc..400000]
    acc = 10000
    rev = cycle $ replicate 100 True ++ replicate 100 False
