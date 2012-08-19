{-# LANGUAGE BangPatterns #-}

module Gamgine.Image.PNG.Internal.CRC (update_crc, crc) where

import Data.Word
import Data.Array.Unboxed
import Data.Bits
import qualified Data.ByteString.Lazy as LB

crc_table :: UArray Word32 Word32
crc_table = listArray (0,255) . map iterate_c $ [0..]
 where
   iterate_c = (!! 8) . iterate compute_c
   compute_c c
       | c .&. 1 == 1   = 0xedb88320 `xor` (c `shiftR` 1)
       | otherwise      = c `shiftR` 1

update_crc :: Word32 -> LB.ByteString -> Word32
update_crc !c bs
    | LB.null bs        = c
    | otherwise         = let w      = LB.head bs
                              newcrc = (crc_table ! ((c `xor` fromIntegral w) .&. 0xff)) `xor` (c `shiftR` 8)
                          in
                            update_crc newcrc (LB.tail bs)

crc :: LB.ByteString -> Word32
crc = (`xor` 0xffffffff) . update_crc 0xffffffff

--test = crc $ LB.replicate 10000000 128
