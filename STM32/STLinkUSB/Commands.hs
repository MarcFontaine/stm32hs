{-# LANGUAGE RecordWildCards #-}
----------------------------------------------------------------------------
-- |
-- Module      :  STM32.Commands
-- Copyright   :  (c) Marc Fontaine 2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- The bits, bytes and constants of the STLink protocoll.
-- The constants have been looked up a corresponding driver that is
-- part of the openocd library.
-- Some parts have been added for completeness (but have not been tested so far). 
module STM32.STLinkUSB.Commands
where
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL (toStrict)
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Bits
       
data Version = Version {
   stlink :: Word16
  ,jtag   :: Word16
  ,swim   :: Word16
  } deriving Show

instance Binary Version where
  put _ = error "Version put not implemented"
  get = do
    v <- getWord16be
    let
      stlink = (v `shiftR` 12) .&. 0x0f 
      jtag   = (v `shiftR` 6)  .&. 0x3f 
      swim   = v .&. 0x3f
    return $ Version {..}

-- | APIV1 is NOT supported ! Todo remove old APIV1 stuff.
-- | todo 
data API = APIV1 | APIV2
  deriving (Show,Eq)

type Addr = Word32
data DebugCmd
  = ENTER_JTAG
  | GETSTATUS
  | FORCEDEBUG
  | READMEM_32BIT  Addr Word16
  | WRITEMEM_32BIT Addr Word16
  | RUNCORE
  | STEPCORE
  | READMEM_8BIT  Addr Word16
  | WRITEMEM_8BIT Addr Word16
  | APIV1_CLEARFP
  | APIV1_SETWATCHPOINT
  | ENTER_SWD
  | EXIT
  | READCOREID
  | APIV1_SETFP
  | ENTER API
  | APIV2_READ_IDCODES
  | RESETSYS API
  | READREG API
  | WRITEREG API
  | WRITEDEBUGREG API Addr Word32
  | APIV2_READDEBUGREG Addr
  | READALLREGS API
  | GETLASTRWSTATUS
  | APIV2_DRIVE_NRST
  | APIV2_START_TRACE_RX Word16 Word32
  | APIV2_STOP_TRACE_RX
  | APIV2_GET_TRACE_NB
  | APIV2_SWD_SET_FREQ
  | APIV2_DRIVE_NRST_LOW
  | APIV2_DRIVE_NRST_HIGH
  | APIV2_DRIVE_NRST_PULSE
  deriving (Show,Eq)

instance Binary DebugCmd where
  get = error "no Binary get for debugCMD"
  put cmd = case cmd of
    ENTER_JTAG            -> putWord8 0x00
    GETSTATUS             -> putWord8 0x01
    FORCEDEBUG            -> putWord8 0x02
    READALLREGS APIV1     -> putWord8 0x04
    READALLREGS APIV2     -> putWord8 0x3A
    READREG APIV1         -> putWord8 0x05
    READREG APIV2         -> putWord8 0x33
    WRITEREG APIV1        -> putWord8 0x06
    WRITEREG APIV2        -> putWord8 0x34
    READMEM_32BIT addr len
      -> putWord8 0x07 >> putWord32le addr >> putWord16le len
    WRITEMEM_32BIT addr len
      -> putWord8 0x08  >> putWord32le addr >> putWord16le len
    RUNCORE               -> putWord8 0x09
    STEPCORE              -> putWord8 0x0a
    APIV1_SETFP           -> putWord8 0x0b
    READMEM_8BIT addr len
      -> putWord8 0x0c >> putWord32le addr >> putWord16le len
    WRITEMEM_8BIT addr len
      -> putWord8 0x0d >> putWord32le addr >> putWord16le len
    APIV1_CLEARFP         -> putWord8 0x0e
    APIV1_SETWATCHPOINT   -> putWord8 0x10
    ENTER_SWD             -> putWord8 0xa3
    (ENTER APIV1)         -> putWord8 0x20
    EXIT                  -> putWord8 0x21
    READCOREID            -> putWord8 0x22
    ENTER APIV2           -> putWord8 0x30
    APIV2_READ_IDCODES    -> putWord8 0x31
    (RESETSYS APIV2)      -> putWord8 0x32
    (RESETSYS APIV1)      -> putWord8 0x03
    (WRITEDEBUGREG APIV1 w1 w2)
         -> putWord8 0x0f >> putWord32le w1 >> putWord32le w2
    (WRITEDEBUGREG APIV2 w1 w2)
         -> putWord8 0x35 >> putWord32le w1 >> putWord32le w2
    APIV2_READDEBUGREG  w -> putWord8 0x36 >> putWord32le w
    GETLASTRWSTATUS -> putWord8 0x3B
    APIV2_DRIVE_NRST      -> putWord8 0x3C
    APIV2_START_TRACE_RX  size speed
          -> putWord8 0x40   >> putWord16le size >> putWord32le speed
    APIV2_STOP_TRACE_RX   -> putWord8 0x41
    APIV2_GET_TRACE_NB    -> putWord8 0x42
    APIV2_SWD_SET_FREQ    -> putWord8 0x43
    APIV2_DRIVE_NRST_LOW   -> putWord8 0x00
    APIV2_DRIVE_NRST_HIGH  -> putWord8 0x01
    APIV2_DRIVE_NRST_PULSE -> putWord8 0x02

data Cmd
  = GET_VERSION
  | DEBUG_COMMAND DebugCmd
  | DEBUG_COMMANDs [DebugCmd]
  | DFU_COMMAND_EXIT
  | SWIM_COMMAND SWIM_Cmd
  | GET_CURRENT_MODE
  | GET_TARGET_VOLTAGE
  deriving Show

data SWIM_Cmd = SWIM_ENTER | SWIM_EXIT
  deriving Show
  
instance Binary Cmd where
  get = error "no Binary get for debugCMD"
  put cmd = case cmd of
    GET_VERSION               -> putWord8 0xF1
    DEBUG_COMMAND  c          -> putWord8 0xF2 >> put c
    DEBUG_COMMANDs l
      -> putWord8 0xF2 >> mapM_ put l
    DFU_COMMAND_EXIT          -> putWord8 0xF3 >> putWord8 0x07
    (SWIM_COMMAND SWIM_ENTER) -> putWord8 0xF4 >> putWord8 0x00
    (SWIM_COMMAND SWIM_EXIT)  -> putWord8 0xF4 >> putWord8 0x01
    GET_CURRENT_MODE          -> putWord8 0xF5
    GET_TARGET_VOLTAGE        -> putWord8 0xF7
  
cmdToByteString :: Cmd -> BS.ByteString
cmdToByteString cmd
  = BS.take 16 $ BSL.toStrict $ runPut (put cmd >> padding)
  where
    padding = putWord64le 0 >> putWord64le 0

data DevMode
  = DEV_DFU_MODE
  | DEV_MASS_MODE
  | DEV_DEBUG_MODE
  | DEV_SWIM_MODE
  | DEV_BOOTLOADER_MODE
  deriving (Show,Eq,Ord,Enum)

data Status
  = DEBUG_ERR_OK
  | DEBUG_ERR_FAULT
  | SWD_AP_WAIT
  | SWD_AP_FAULT
  | SWD_AP_ERROR
  | SWD_AP_PARITY_ERROR
  | JTAG_WRITE_ERROR
  | JTAG_WRITE_VERIF_ERROR
  | SWD_DP_WAIT
  | SWD_DP_FAULT
  | SWD_DP_ERROR
  | SWD_DP_PARITY_ERROR
  | SWD_AP_WDATA_ERROR
  | SWD_AP_STICKY_ERROR
  | SWD_AP_STICKYORUN_ERROR
  | UnknownStatus Word8
  deriving (Show,Eq,Ord)

toStatus :: Word8 -> Status
toStatus w = case w of
  0x80 -> DEBUG_ERR_OK
  0x81 -> DEBUG_ERR_FAULT
  0x10 -> SWD_AP_WAIT
  0x11 -> SWD_AP_FAULT
  0x12 -> SWD_AP_ERROR
  0x13 -> SWD_AP_PARITY_ERROR
  0x0c -> JTAG_WRITE_ERROR
  0x0d -> JTAG_WRITE_VERIF_ERROR
  0x14 -> SWD_DP_WAIT
  0x15 -> SWD_DP_FAULT
  0x16 -> SWD_DP_ERROR
  0x17 -> SWD_DP_PARITY_ERROR
  0x18 -> SWD_AP_WDATA_ERROR
  0x19 -> SWD_AP_STICKY_ERROR
  0x1a -> SWD_AP_STICKYORUN_ERROR
  other -> UnknownStatus other
