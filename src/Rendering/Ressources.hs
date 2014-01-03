{-# LANGUAGE TupleSections #-}

module Rendering.Ressources where
import Control.Applicative ((<$>))
import Data.Maybe (fromJust)
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Font.GLF as GLF
import qualified Gamgine.Gfx as G
import qualified Gamgine.State.RenderState as RS
import qualified Ressources as R

data Texture = Background | Player | Enemy | Star deriving (Show, Eq, Enum)
data Font    = Crystal | Courier deriving (Show, Eq, Enum)

textureId :: Texture -> RS.Ressources -> GL.GLuint
textureId tex = fromJust . RS.textureId (textureName tex)

fontId :: Font -> RS.Ressources -> GLF.FontId
fontId font = fromJust . RS.fontId (fontName font)

textureName :: Texture -> RS.TextureName
textureName = RS.TextureName . fromEnum

fontName :: Font -> RS.FontName
fontName = RS.FontName . fromEnum

newRessources :: IO RS.Ressources
newRessources = do
   texIds  <- mapM (\(tex, file) -> (textureName tex,) <$> mkTexture file) textures
   fontIds <- mapM (\(font, file) -> (fontName font,) <$> mkFont file) fonts
   return $ RS.Ressources texIds fontIds
   where
      textures = [(Background, "Background.png"),
                  (Player    , "Player.png"),
                  (Enemy     , "Enemy.png"),
                  (Star      , "Star.png")]

      fonts = [(Crystal, "crystal1.glf"), (Courier, "courier1.glf")]

      mkTexture file = do
         tex <- R.getImageFilePath file
         G.makeTexture2d tex GL.gl_REPEAT

      mkFont file = do
         font <- R.getFontFilePath file
         GLF.loadFont font


emptyRessources :: RS.Ressources
emptyRessources = RS.Ressources [] []
