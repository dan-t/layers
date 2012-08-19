{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Gamgine.Gfx where
import Graphics.Rendering.OpenGL.Raw
import Control.Monad (forM_)
import Data.Either
import Foreign.Marshal.Utils
import Foreign.Storable
import Data.Array.Storable
import System.IO
import Gamgine.Image.PNG
import Gamgine.Math.Box
import Gamgine.Math.Vect
import Gamgine.Math.BoxTree as BT
import Gamgine.Utils
#include "Gamgine/Utils.cpp"

type RGB  = (Double, Double, Double)
type RGBA = (Double, Double, Double, Double)
type TexCoord = (Double, Double)

floatToFloat :: (RealFloat a, RealFloat b) => a -> b
floatToFloat = (uncurry encodeFloat) . decodeFloat

f <<* (a, b) = f (floatToFloat a) (floatToFloat b)
infixl 5 <<*

f <<<* (a, b, c) = f (floatToFloat a) (floatToFloat b) (floatToFloat c)
infixl 5 <<<*

f <<<<* (a, b, c, d) = f (floatToFloat a) (floatToFloat b) (floatToFloat c) (floatToFloat d)
infixl 5 <<<<*

f <<< (a:.b:.c:.()) = f (floatToFloat a) (floatToFloat b) (floatToFloat c)
infixl 5 <<<

class GLVertex a where
   vertex :: a -> IO ()

instance GLVertex Vect where
   vertex v = glVertex3f <<< v

instance GLVertex (Double,Double,Double) where
   vertex v = glVertex3f <<<* v

quad :: (Double,Double) -> (Double,Double) -> [(Double,Double)]
quad (minx, miny) (maxx, maxy) =
   [(minx,miny), (maxx,miny), (maxx, maxy), (minx, maxy)]

quadTexCoords :: Double -> Double -> [(Double,Double)]
quadTexCoords maxx maxy = [(0,maxy), (maxx,maxy), (maxx,0), (0,0)]

draw :: GLVertex a => GLenum -> [a] -> IO ()
draw primType vertices = withPrimitive primType $ mapM_ (\v -> vertex v) vertices

drawBox :: Box -> IO ()
drawBox box = do
   drawQuad (minPt box) (maxPt box)

drawQuad :: Vect -> Vect -> IO ()
drawQuad (minX:.minY:._) (maxX:.maxY:._) = do
   draw gl_QUADS [(minX, minY, 0 :: Double), (maxX, minY, 0 :: Double),
                  (maxX, maxY, 0 :: Double), (minX, maxY, 0 :: Double)]

drawBoxTree :: BT.BoxTree a -> IO ()
drawBoxTree tree = do
   go tree
   where
      go (Node box ts) = drawBox box >> mapM_ (\t -> go t) ts
      go (Leaf box _)  = drawBox box

drawPoint :: Vect -> RGB -> IO ()
drawPoint pos color = do
   glPointSize 10
   glBegin gl_POINTS
   glVertex3f <<< pos
   glEnd

withPrimitive :: GLenum -> IO () -> IO ()
withPrimitive primType act = do
   glBegin primType
   act
   glEnd

withPushedMatrix :: IO () -> IO ()
withPushedMatrix act = do
   glPushMatrix
   act
   glPopMatrix

withPolyMode :: GLenum -> IO () -> IO ()
withPolyMode mode act = do
   glPolygonMode gl_FRONT_AND_BACK mode
   act
   glPolygonMode gl_FRONT_AND_BACK gl_FILL

withEnabled :: GLenum -> IO () -> IO ()
withEnabled mode act = do
   glEnable mode
   act
   glDisable mode

withBlend :: GLenum -> GLenum -> IO () -> IO ()
withBlend srcFactor dstFactor act = do
   glBlendFunc srcFactor dstFactor
   withEnabled gl_BLEND act

withTexture2d :: GLuint -> IO () -> IO ()
withTexture2d id act = do
   glBindTexture gl_TEXTURE_2D id
   withEnabled gl_TEXTURE_2D act

makeTexture2d :: FilePath -> GLenum -> IO GLuint
makeTexture2d file wrapMode = do
   res <- loadPNGFile file
   either (\str -> ERROR str)
          (\img -> genTex img)
	  res
   where
      genTex img = do
	 let (width, height) = dimensions img
             imgData         = imageData img
	     format          = hasAlphaChannel img ? gl_RGBA $ gl_RGB
	 id <- with 0 (\buf -> glGenTextures 1 buf >> peek buf)
	 glBindTexture gl_TEXTURE_2D id
	 glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_S (fromIntegral wrapMode)
	 glTexParameteri gl_TEXTURE_2D gl_TEXTURE_WRAP_T (fromIntegral wrapMode)
	 glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER (fromIntegral gl_NEAREST)
	 glTexParameteri gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER (fromIntegral gl_NEAREST)
	 withStorableArray imgData (\array ->
	    glTexImage2D gl_TEXTURE_2D 0 (fromIntegral format) (fromIntegral width)
	                 (fromIntegral height) 0 (fromIntegral format) gl_UNSIGNED_BYTE array)
	 return id


renderTexturedQuad :: (Double,Double) -> Vect -> GLuint -> IO ()
renderTexturedQuad size translation texture =
   withPushedMatrix $ do
      glTranslatef <<< translation
      withTexture2d texture $
         withBlend gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA $
            withPrimitive gl_QUADS $ do
               let coords   = quadTexCoords 1 1
                   vertices = quad (0,0) size
               glColor3f <<<* (1,1,1)
               forM_ (zip coords vertices) (\(c,v) -> do
                  glTexCoord2f <<* c
                  glVertex2f <<* v)
