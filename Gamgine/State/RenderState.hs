
module Gamgine.State.RenderState where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import qualified Data.List as L
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Font.GLF as GLF
IMPORT_LENS_AS_LE

newtype TextureName = TextureName Int deriving (Show, Eq)
newtype FontName    = FontName Int deriving (Show, Eq)

type TextureIds = [(TextureName, GL.GLuint)]
type FontIds    = [(FontName   , GLF.FontId)]

data Ressources = Ressources {
   textureIds :: TextureIds,
   fontIds    :: FontIds
   } deriving Show

emptyRessources :: Ressources
emptyRessources = Ressources [] []

textureId :: TextureName -> Ressources -> Maybe GL.GLuint
textureId name res = L.lookup name $ textureIds res

fontId :: FontName -> Ressources -> Maybe GLF.FontId
fontId name res = L.lookup name $ fontIds res

LENS(textureIds)
LENS(fontIds)

data RenderState = RenderState {
   nextFrameFraction :: Double,    -- ^ value range 0-1
   ressources        :: Ressources,
   frustumSize       :: (Double, Double)
   } deriving Show

LENS(nextFrameFraction)
LENS(ressources)
LENS(frustumSize)
