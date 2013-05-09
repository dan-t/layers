
module Level.Render where
#include "Gamgine/Utils.cpp"
import Data.Foldable (foldrM)
import Control.Monad (forM_, mapM_)
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Ressources as R
import qualified Gamgine.Gfx as G
import Gamgine.Math.Vect as V
import qualified Gamgine.State.RenderState as RS
import qualified GameData.Level as LV
import qualified GameData.Layer as LY
import qualified GameData.Boundary as BD
import qualified GameData.Entity as E
import qualified Rendering.Renderer as RD
import qualified Rendering.Ressources as RR
import qualified Entity.Render as ER
import Gamgine.Gfx ((<<<), (<<))
IMPORT_LENS_AS_LE


render :: RS.RenderState -> LV.Level -> IO LV.Level
render rstate level = do
   renderBackground (bgx, bgy) bgTexId
   renderInactLayer
   renderActLayer
   renderEntities
   rs' <- runRenderers rstate $ LV.renderers level
   return level {LV.renderers = rs'}
   where
      renderInactLayer = forM_ inactLays $ (mapM_ $ ER.render E.InactiveLayerScope rstate) . LY.entities
      renderActLayer   = mapM_ (ER.render E.ActiveLayerScope rstate) $ LY.entities actLayer
      renderEntities   = mapM_ (ER.render E.LevelScope rstate) $ LV.entities level
      inactLays        = LV.inactiveLayers level
      actLayer         = LE.getL LV.activeLayerL level
      (bgx:.bgy:._)    = BD.boundaryArea . LV.boundary $ level
      bgTexId          = RR.textureId RR.Background $ RS.ressources rstate


renderBackground :: (Double,Double) -> GL.GLuint -> IO ()
renderBackground (sizeX, sizeY) texId  = do
   G.withTexture2d texId $
      G.withPrimitive GL.gl_QUADS $ do
         let coords   = G.quadTexCoords 100 100
             vertices = G.quad (0,0) (max 2000 sizeX, max 1000 sizeY)
         GL.glColor3f <<< G.rgb 1 1 1
         forM_ (zip coords vertices) (\(c,v) -> do
            GL.glTexCoord2f << c
            GL.glVertex2f << v)


runRenderers :: RS.RenderState -> [RD.Renderer] -> IO [RD.Renderer]
runRenderers renderState rs = do
   foldrM (\r rs -> do
      (finished, r') <- RD.runRenderer r renderState
      if finished
         then return rs
         else return $ r' : rs) [] rs
