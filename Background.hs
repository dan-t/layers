
module Background where
import Control.Monad (forM_)
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Ressources as R
import qualified Gamgine.Gfx as G
import Gamgine.Gfx ((<<<*), (<<*))


data Background = Background {
   size      :: (Double, Double),
   textureId :: GL.GLuint
   } deriving Show


newBackground :: IO Background
newBackground = do
   fp  <- R.getImageFilePath "Background.png"
   tex <- G.makeTexture2d fp GL.gl_REPEAT
   return Background {size = (2000, 1000), textureId = tex}


empty :: Background
empty = Background {size = (0,0), textureId = 0}


render :: Background -> IO ()
render Background {size = size, textureId = texId} = do
   G.withTexture2d texId $
      G.withPrimitive GL.gl_QUADS $ do
         let coords   = G.quadTexCoords 100 100
             vertices = G.quad (0,0) size
         GL.glColor3f <<<* (1,1,1)
         forM_ (zip coords vertices) (\(c,v) -> do
            GL.glTexCoord2f <<* c
            GL.glVertex2f <<* v)
