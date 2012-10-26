
module Rendering.Ressources where
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as G
import qualified Gamgine.Ressources as R

-- | render ressources of entities
data Ressources = Ressources {
   playerTextureId :: GL.GLuint,
   starTextureId   :: GL.GLuint
   } deriving Show


data RenderState = RenderState {
   nextFrameFraction :: Double,    -- ^ value range 0-1
   ressources        :: Ressources
   } deriving Show


newRessources :: IO Ressources
newRessources = do
   playTex   <- R.getImageFilePath "Player.png"
   starTex   <- R.getImageFilePath "Star.png"
   playTexId <- G.makeTexture2d playTex GL.gl_REPEAT
   starTexId <- G.makeTexture2d starTex GL.gl_REPEAT
   return $ Ressources playTexId starTexId
