{-# LANGUAGE ForeignFunctionInterface #-}

module GLF where
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal.Alloc
import Control.Applicative ((<$>))


init :: IO ()
init = c_glfInit


newtype FontId = FontId Int deriving (Eq, Show)

loadFont :: FilePath -> IO FontId
loadFont s = do
   withCString s $ \cs -> do
      ci <- c_glfLoadFont cs
      return . FontId . fromIntegral $ ci


setCurrentFont :: FontId -> IO Bool
setCurrentFont (FontId id) = do
   ci <- c_glfSetCurrentFont $ fromIntegral id
   return $ glfOk == (fromIntegral ci)


type X = Double
type Y = Double

data Bounds = Bounds {
   min :: (X, Y),
   max :: (X, Y)
   } deriving Show

getStringBounds :: String -> IO Bounds
getStringBounds s = do
   withCString s $ \cs ->
      alloca $ \cminX ->
      alloca $ \cminY ->
      alloca $ \cmaxX ->
      alloca $ \cmaxY -> do
         c_glfGetStringBounds cs cminX cminY cmaxX cmaxY
         minX <- peekToFrac cminX
         minY <- peekToFrac cminY
         maxX <- peekToFrac cmaxX
         maxY <- peekToFrac cmaxY
         return $ Bounds (minX, minY) (maxX, maxY)
   where
      peekToFrac = (realToFrac <$>) . peek


drawWiredString :: String -> IO ()
drawWiredString s = withCString s c_glfDrawWiredString


drawSolidString :: String -> IO ()
drawSolidString s = withCString s c_glfDrawSolidString



glfError :: Int
glfError = -1

glfOk :: Int
glfOk = 0

glfYes :: Int
glfYes = 1

glfNo :: Int
glfNo = 2


foreign import ccall unsafe "glf.h glfInit"
   c_glfInit :: IO ()

foreign import ccall unsafe "glf.h glfLoadFont"
   c_glfLoadFont :: CString -> IO CInt

foreign import ccall unsafe "glf.h glfSetCurrentFont"
   c_glfSetCurrentFont :: CInt -> IO CInt

foreign import ccall unsafe "glf.h glfGetStringBounds"
   c_glfGetStringBounds :: CString -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> Ptr CFloat -> IO ()

foreign import ccall unsafe "glf.h glfDrawWiredString"
   c_glfDrawWiredString :: CString -> IO ()

foreign import ccall unsafe "glf.h glfDrawSolidString"
   c_glfDrawSolidString :: CString -> IO ()
