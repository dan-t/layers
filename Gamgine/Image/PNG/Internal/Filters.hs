{-# LANGUAGE BangPatterns #-}

module Gamgine.Image.PNG.Internal.Filters (defilter_scanlines_arr) where

import Data.Array.Storable
import Data.Array.IO

import Data.Word
import Data.Maybe

import qualified Data.ByteString.Lazy as LB

type Width = Int
type Height = Int

-- | Defilter filtered PNG data (data includes scanlines prepended with filter
--   types).
defilter_scanlines_arr :: (Width,Height) -> Int -> LB.ByteString
                       -> IO (StorableArray (Width,Height) Word8)
defilter_scanlines_arr (width,height) bpp bs = do
  (slTypes, imgArr) <- imageArray (widthInBytes, height) bs
  doFilter widthInBytes bpp imgArr slTypes
  return imgArr
 where
   widthInBytes = bpp*width

-- | create mutable array for defiltering the image and break out scanline types
imageArray :: (Width,Height) -> LB.ByteString
           -> IO ([Word8],StorableArray (Int,Int) Word8)
imageArray (width,height) bs = do
  a <- newListArray ((0,0), (height-1, width-1)) $ LB.unpack imageData
  return (scanlineTypes, a)
 where
   imageData = LB.concat scanlineData
   (scanlineTypes, scanlineData) = unzip scanlines
   scanlines = map (fromJust . LB.uncons) (chop bs)
   chop b
       | LB.null b   = []
       | otherwise   = let (sl,rest) = LB.splitAt slWidth b
                       in sl : chop rest
   slWidth = fromIntegral (width+1)

-- | Perform filtering on the image array (standard adaptive filters supported)
doFilter :: Width -> Int -> StorableArray (Int,Int) Word8 -> [Word8]
         -> IO ()
doFilter width bpp image scanlineTypes = doFilter' scanlineTypes 0
 where
   doFilter'       [] _ = return ()
   doFilter' (0:rest) !y = doFilter' rest (y+1)        -- no filter on this line
   doFilter' (1:rest) !y = sub_filter 0 >> doFilter' rest (y+1)
     where sub_filter !x
               | x<width = do subx <- readByte (y,x)
                              raw  <- readByte (y,x-bpp)
                              writeByte (y,x) (subx+raw)
                              sub_filter (x+1)
               | otherwise = return ()
   doFilter' (2:rest) !y = up_filter 0 >> doFilter' rest (y+1)
     where up_filter !x
               | x<width = do upx <- readByte (y,x)
                              prior <- readByte (y-1,x)
                              writeByte (y,x) (upx+prior)
                              up_filter (x+1)
               | otherwise = return ()
   doFilter' (3:rest) !y = avg_filter 0 >> doFilter' rest (y+1)
     where avg_filter !x
               | x<width = do avgx <- readByte (y,x)
                              raw <- readByte (y,x-bpp)
                              prior <- readByte (y-1,x)
                              let s = ((fromIntegral raw + fromIntegral prior) `div` (2::Word16))
                              writeByte (y,x) (avgx + fromIntegral s)
                              avg_filter (x+1)
               | otherwise = return ()
   doFilter' (4:rest) !y = paeth_filter 0 >> doFilter' rest (y+1)
     where paeth_filter !x
               | x<width = do paethx <- readByte (y,x)
                              a <- readByte (y,x-bpp)
                              b <- readByte (y-1,x)
                              c <- readByte (y-1, x-bpp)
                              writeByte (y,x) (paethx + fromIntegral (paeth_predictor (fromIntegral a) (fromIntegral b) (fromIntegral c)))
                              paeth_filter (x+1)
               | otherwise = return ()
   doFilter' (_:rest) !y = doFilter' rest (y+1) -- unknown filter, leave as is (and mess up image ;)
   {-# INLINE readByte #-}
   readByte (!y,!x) = if x<0 then return 0 else readArray image (y,x)
   {-# INLINE writeByte #-}
   writeByte = writeArray image

{-# INLINE paeth_predictor #-}
paeth_predictor :: Int -> Int -> Int -> Int
paeth_predictor !a !b !c
    | pa <= pb && pa <= pc  = a
    | pb <= pc              = b
    | otherwise             = c
 where
   p = a + b - c
   pa = abs(p-a)
   pb = abs(p-b)
   pc = abs(p-c)
