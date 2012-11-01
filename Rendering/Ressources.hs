
module Rendering.Ressources where
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as G
import qualified Gamgine.Ressources as R

-- | render ressources of entities
data Ressources = Ressources {
   backgroundTextureId :: GL.GLuint,
   playerTextureId     :: GL.GLuint,
   starTextureId       :: GL.GLuint
   } deriving Show


data RenderState = RenderState {
   nextFrameFraction :: Double,    -- ^ value range 0-1
   ressources        :: Ressources
   } deriving Show


newRessources :: IO Ressources
newRessources = do
   backId    <- mkTexture "Background.png"
   playTexId <- mkTexture "Player.png"
   starTexId <- mkTexture "Star.png"
   return $ Ressources backId playTexId starTexId
   where
      mkTexture file = do
         tex <- R.getImageFilePath file
         G.makeTexture2d tex GL.gl_REPEAT
