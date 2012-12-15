
module Rendering.Ressources where
#include "Gamgine/Utils.cpp"
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as G
import qualified Gamgine.Ressources as R
import qualified GLF
IMPORT_LENS

-- | render ressources of entities
data Ressources = Ressources {
   backgroundTextureId :: GL.GLuint,
   playerTextureId     :: GL.GLuint,
   starTextureId       :: GL.GLuint,
   crystalFontId       :: GLF.FontId,
   courierFontId       :: GLF.FontId
   } deriving Show

LENS(backgroundTextureId)
LENS(playerTextureId)
LENS(starTextureId)
LENS(crystalFontId)
LENS(courierFontId)


data RenderState = RenderState {
   nextFrameFraction :: Double,    -- ^ value range 0-1
   ressources        :: Ressources,
   frustumSize       :: (Double, Double)
   } deriving Show

LENS(nextFrameFraction)
LENS(ressources)
LENS(frustumSize)


newRessources :: IO Ressources
newRessources = do
   backId    <- mkTexture "Background.png"
   playTexId <- mkTexture "Player.png"
   starTexId <- mkTexture "Star.png"
   crystalId <- mkFont "crystal1.glf"
   courierId <- mkFont "courier1.glf"
   return $ Ressources backId playTexId starTexId crystalId courierId
   where
      mkTexture file = do
         tex <- R.getImageFilePath file
         G.makeTexture2d tex GL.gl_REPEAT

      mkFont file = do
         font <- R.getFontFilePath file
         GLF.loadFont font


emptyRessources :: Ressources
emptyRessources = Ressources (-1) (-1) (-1) (GLF.FontId (-1)) (GLF.FontId (-1))
