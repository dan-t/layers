-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.Formats.PNG
-- Copyright   :  (c) Marko Lauronen 2008
-- License     :  BSD
-- 
-- Maintainer  :  marko.lauronen@pp1.inet.fi
-- Stability   :  experimental
-- Portability :  non-portable (GHC only)
--
-- A simple, pure Haskell PNG loader. Currently supports 24bit RGB(A) images
-- with no interlacing. Also lacks support for color indexed (paletted) images.
--
-- The image is stored in a StorableArray for compatibility with OpenGL (the
-- array supports getting Ptr Word8 to the image data using withStorableArray
-- function).
--
-----------------------------------------------------------------------------
module Gamgine.Image.PNG
    (
     -- * Types
      PNGImage, Width, Height
     -- * Functions
    , loadPNGFile
    , dimensions
    --, pixelWidth
    , imageData
    , hasAlphaChannel
    ) where

import Codec.Compression.Zlib

import Text.Parsec.Combinator
import Text.Parsec.Prim

import Data.Array.Unboxed
import Data.Array.Storable
import Data.Word
import Data.List
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Int
import Data.Char
import System.IO

import Control.Monad.Error

import Gamgine.Image.PNG.Internal.Parser
import Gamgine.Image.PNG.Internal.CRC
import Gamgine.Image.PNG.Internal.Filters

-- | Type for raw PNG chunks
-- The parsing happens in two phases: first the file is read into
-- raw chunks, then the raw chunks are parsed into actual PNGChunks
-- This is due to the CRC checksum that requires is easiest to compute
-- with raw chunk data.
data RawPNGChunk = RawPNGChunk {
      rawPngChunk_type  :: !String,
      rawPngChunk_data  :: !LB.ByteString
    } deriving (Show)

type Width  = Word32
type Height = Word32
type Rgb = (Word8, Word8, Word8)

-- | The actual fully parsed chunk type
data PNGChunk =
    IHDR {
      ihdr_width             :: !Width
    , ihdr_height            :: !Height
    , ihdr_bitDepth          :: !BitDepth
    , ihdr_colorType         :: !ColorType
    , ihdr_compressionMethod :: !CompressionMethod
    , ihdr_filterMethod      :: !FilterMethod
    , ihdr_interlaceMethod   :: !InterlaceMethod }
  | PLTE {
      plte_entries :: !(Array Word8 Rgb) }
  | IDAT {
      idat_data :: !LB.ByteString }
  | UnknownChunk RawPNGChunk    -- chunk types not supported yet
  | IEND
    deriving (Show)

data ColorType         = Ct0 | Ct2 | Ct3 | Ct4 | Ct6 deriving (Show,Eq)
data BitDepth          = Bd1 | Bd2 | Bd4 | Bd8 | Bd16 deriving (Show,Eq)
data CompressionMethod = Deflate deriving (Show,Eq)
data FilterMethod      = Adaptive deriving (Show,Eq)
data InterlaceMethod   = NoInterlace | Adam7 deriving (Show,Eq)

isIDAT :: PNGChunk -> Bool
isIDAT (IDAT _) = True
isIDAT _        = False

data PNGImage = PNGImage {
      pngImg_header  :: !PNGChunk
    , pngImg_otherChunks :: ![PNGChunk]
    , pngImg_imageData :: !(StorableArray (Int,Int) Word8)
    }

instance Show PNGImage where
    show _ = "PNGImage"

-- | Raw chunk parsing

pngHeaderBytes :: LB.ByteString
pngHeaderBytes = LB.pack [137, 80, 78, 71, 13, 10, 26, 10]

pngFile :: Parser [RawPNGChunk]
pngFile = do
  string pngHeaderBytes
  hdr <- rawPngChunk
  when (rawPngChunk_type hdr /= "IHDR") $
       fail "expecting IHDR as the first chunk"
  rest <- many1 rawPngChunk
  return (hdr:rest)

rawPngChunk :: Parser RawPNGChunk
rawPngChunk = do
  len <- anyWord32
  chunkType <- block 4
  chunkData <- block (fromIntegral len)
  let expectedCrc = crc (LB.concat [chunkType,chunkData])
  word32 expectedCrc <?> "valid crc"
  return $ RawPNGChunk (C.unpack chunkType) chunkData

-- | Final chunk parsing

parsePlte :: Parser PNGChunk
parsePlte = do
  paletteEntries <- many1 paletteEntry
  return . PLTE $ listArray (0, fromIntegral (length paletteEntries-1)) paletteEntries
 where
   paletteEntry = liftM3 (,,) anyWord8 anyWord8 anyWord8

parseIhdr :: Parser PNGChunk
parseIhdr = do
  width <- anyWord32
  height <- anyWord32
  -- [(1,Bd1), (2,Bd2), (4,Bd4), (8,Bd8), (16,Bd16)]
  bitDepth <- allowedValues word8 [(8,Bd8)]
              <?> "valid bit depth (supported: Bd8)"
  --[(0,Ct0), (2,Ct2), (3,Ct3), (4,Ct4), (6,Ct6)]
  colorType <- allowedValues word8 [(2,Ct2), (6,Ct6)]
               <?> "valid colorType: supported Ct2,Ct6"
  compressionMethod <- allowedValues word8 [(0, Deflate)] 
                       <?> "valid compression method: supported Deflate"
  filterMethod <- allowedValues word8 [(0, Adaptive)] 
                  <?> "valid filter method: supported Adaptive"
  -- [(0, NoInterlace), (1, Adam7)] 
  interlaceMethod <- allowedValues word8 [(0, NoInterlace)] 
                     <?> "valid interlace method: supported NoInterlace"
  return $ IHDR {
               ihdr_width = width
             , ihdr_height = height
             , ihdr_bitDepth = bitDepth
             , ihdr_colorType = colorType
             , ihdr_compressionMethod = compressionMethod
             , ihdr_filterMethod = filterMethod
             , ihdr_interlaceMethod = interlaceMethod
             }

-- | conversion from raw chunks to final chunks
toPngChunk :: RawPNGChunk -> Either String PNGChunk
toPngChunk raw =
    case chunkName of
      "IHDR"   -> parseChunkData parseIhdr
      "PLTE"   -> parseChunkData parsePlte
      "IEND"   -> return IEND
      "IDAT"   -> return $ IDAT (rawPngChunk_data raw)
      _        -> return $ UnknownChunk raw
 where
   parseChunkData a =
       case runP a () "" (rawPngChunk_data raw) of
         Left e  -> fail $ "failed to parse chunk " ++ show chunkName ++ ", " ++ show e
         Right x -> return x
   chunkName = rawPngChunk_type raw

toPngImage :: [RawPNGChunk] -> IO (Either String PNGImage)
toPngImage chunks = do
  case mapM toPngChunk chunks >>= return . partition isIDAT of
    Right (_, []) -> return $ Left "File has no chunks!"
    Right (dataChunks, hdr:otherChunks)  -> do
                      let dataDecompressed = decompress . LB.concat . map idat_data $ dataChunks
                          bpp = bytesPerPixel (ihdr_colorType hdr) (ihdr_bitDepth hdr)
                          w = fromIntegral (ihdr_width hdr)
                          h = fromIntegral (ihdr_height hdr)
                      sls <- defilter_scanlines_arr (w,h) (fromIntegral bpp) dataDecompressed
                      return $ Right (PNGImage hdr otherChunks sls)
    Left x -> return $ Left x

-- |Load a PNG file, Left value contains a description of a problem as a String,
-- if any
loadPNGFile :: FilePath -> IO (Either String PNGImage)
loadPNGFile fn = do
  rawChunks <- parseFromFile pngFile fn
  case rawChunks of
    Right chunks  -> toPngImage chunks `catchError` (\e -> return (Left (show e)))
    Left s        -> return (Left s)

sampleWidth :: BitDepth -> Int
sampleWidth Bd1  = 1
sampleWidth Bd2  = 2
sampleWidth Bd4  = 4
sampleWidth Bd8  = 8
sampleWidth Bd16 = 16

bytesPerPixel :: ColorType -> BitDepth -> Int
bytesPerPixel Ct0 Bd16   = 2
bytesPerPixel Ct0 _      = 1
bytesPerPixel Ct2 Bd1    = 1
bytesPerPixel Ct2 Bd2    = 1
bytesPerPixel Ct2 Bd4    = 2
bytesPerPixel Ct2 Bd8    = 3
bytesPerPixel Ct2 Bd16   = 6
bytesPerPixel Ct3 _      = 3
bytesPerPixel Ct4 Bd8    = 2
bytesPerPixel Ct4 Bd16   = 4
bytesPerPixel Ct4 _      = 1
bytesPerPixel Ct6 Bd8    = 4
bytesPerPixel Ct6 Bd16   = 8
bytesPerPixel Ct6 Bd4    = 2
bytesPerPixel Ct6 _      = 1

-- |Check if the image has alpha channel
hasAlphaChannel :: PNGImage -> Bool
hasAlphaChannel img = case ihdr_colorType hdr of
                        Ct6   -> True
                        _     -> False
 where hdr = pngImg_header img

-- |Get dimensions of the image (in pixels)
dimensions :: PNGImage -> (Width,Height)
dimensions img = (ihdr_width hdr, ihdr_height hdr)
 where hdr = pngImg_header img

-- |Bytes per pixel
pixelWidth :: PNGImage -> Int
pixelWidth img = bytesPerPixel (ihdr_colorType hdr) (ihdr_bitDepth hdr)
 where hdr = pngImg_header img

-- |Get image data as C-compatible StorableArray
imageData :: PNGImage -> StorableArray (Int,Int) Word8
imageData img = pngImg_imageData img
