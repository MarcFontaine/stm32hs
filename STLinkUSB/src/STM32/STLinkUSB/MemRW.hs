----------------------------------------------------------------------------
-- |
-- Module      :  STM32.STLinkUSB.MemRW
-- Copyright   :  (c) Marc Fontaine 2017-2022
-- License     :  BSD3
--
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Read and Write to the memory of an attached STM32 controller.

{-# LANGUAGE RankNTypes #-}
module STM32.STLinkUSB.MemRW
where
import Control.Monad
import qualified Data.ByteString as BS
import Data.Binary
import Control.Monad.Trans.Reader

import STM32.STLinkUSB.Commands
import STM32.STLinkUSB.Env
import STM32.STLinkUSB.USBXfer

checkRWStatus :: STL ()
checkRWStatus = do
  api <- asks dongleAPI
  case api of
    APIV1 -> return ()
    APIV2 -> do
      msg <- xfer (DEBUG_COMMAND GETLASTRWSTATUS)
      let dongleStatus = toStatus $ BS.head msg
      if dongleStatus == DEBUG_ERR_OK
         then return ()
         else do
           let err = show ("checkRWStatus", dongleStatus)
           debugSTL Error err
           error err

maxTransferBlocksize :: Word16
maxTransferBlocksize = 64

newtype TransferBlock
  = TransferBlock {_unTransferBlock :: BS.ByteString} deriving Show

unsafeToTransferBlock :: BS.ByteString -> TransferBlock
unsafeToTransferBlock bs
  = if len <= fromIntegral maxTransferBlocksize
       then TransferBlock bs
       else error msg
  where
    msg = "unsafeToTransferBlock :" ++ show len ++ "> maxTransferBlockSize"
    len = BS.length bs

newtype TransferLen = TransferLen {_unTransferLen :: Word16} deriving Show

unsafeToTransferLen :: Word16 -> TransferLen
unsafeToTransferLen len
  = if len <= maxTransferBlocksize
       then TransferLen len
       else error msg
  where
    msg = "unsafeToTransferLen :" ++ show len ++ "> maxTransferBlocksize"

writeMem8' :: Addr -> TransferBlock -> STL ()
writeMem8' addr (TransferBlock block) = do
  void $ xferBulkWrite (DEBUG_COMMAND $ WRITEMEM_8BIT addr len) block
  checkRWStatus
  where
    len = fromIntegral $ BS.length block

writeMem32' :: Addr -> TransferBlock -> STL ()
writeMem32' addr (TransferBlock block) = do
  void $ xferBulkWrite (DEBUG_COMMAND $ WRITEMEM_32BIT addr len) block
  checkRWStatus
  where
    len = fromIntegral $ BS.length block

readMem8' :: Addr -> TransferLen -> STL BS.ByteString
readMem8' addr (TransferLen len) = do
  bs <- xfer (DEBUG_COMMAND $ READMEM_8BIT addr len)
  checkRWStatus
  return bs

readMem32' :: Addr -> TransferLen -> STL BS.ByteString
readMem32' addr (TransferLen len) = do
  bs <- xfer (DEBUG_COMMAND $ READMEM_32BIT addr len)
  checkRWStatus
  return bs

writeMem8 :: Addr -> BS.ByteString -> STL ()
writeMem8 = writeChunks writeMem8'

writeMem32 :: Addr -> BS.ByteString -> STL ()
writeMem32 = writeChunks writeMem32'

writeChunks
  :: (Addr -> TransferBlock -> STL () ) -> Addr -> BS.ByteString -> STL ()
writeChunks action addr bs
  = forM_ (chunkBS addr bs) $ uncurry action


chunkBS :: Addr -> BS.ByteString -> [(Addr,TransferBlock)]
chunkBS addr bs
  = if BS.length bs <= chunkSize
       then [h]
       else h : chunkBS (addr + fromIntegral chunkSize)
                        (BS.drop chunkSize bs)
   where
    h = (addr, unsafeToTransferBlock $ BS.take chunkSize bs)
    chunkSize = fromIntegral maxTransferBlocksize

chunkAddr :: Addr -> Int -> [(Addr,TransferLen)]
chunkAddr addr len
  = if len <= chunkSize
       then [h]
       else h : (chunkAddr (addr + fromIntegral chunkSize)
                      (len - chunkSize))
   where
    h = (addr, unsafeToTransferLen
                    (min (fromIntegral len) (fromIntegral maxTransferBlocksize)))
    chunkSize = fromIntegral maxTransferBlocksize

readChunks
  :: (Addr -> TransferLen -> STL BS.ByteString )
      -> Addr -> Int -> STL BS.ByteString
readChunks action addr len
  =  BS.concat <$> forM (chunkAddr addr len) (uncurry action)

readMem8 :: Addr -> Int -> STL BS.ByteString
readMem8 = readChunks readMem8'

readMem32 :: Addr -> Int -> STL BS.ByteString
readMem32 = readChunks readMem32'
