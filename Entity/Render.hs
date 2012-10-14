
module Entity.Render where
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Ressources as R
import qualified Gamgine.Gfx as G
import qualified Gamgine.Math.Box as B
import qualified Gamgine.Math.Vect as V
import Gamgine.Gfx ((<<<*), (<<<<*), (<<<))
import qualified GameData.Entity as E
import qualified GameData.Player as P
import qualified GameData.Star as S
import qualified GameData.Animation as A
import qualified Utils as LU


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


interpolatePlaformPos :: Double -> E.PositionOrAnimation -> V.Vect
interpolatePlaformPos _ (Left pos) = pos

interpolatePlaformPos nextFrameFraction (Right ani) =
   LU.interpolateFrame nextFrameFraction (A.currentPosition ani) (A.currentVelocity ani)


render :: E.Scope ->
          RenderState ->
          E.Entity ->
          IO ()

render _
       RenderState {nextFrameFraction = frac, ressources = res}
       E.Player {E.playerPosition = pos, E.playerVelocity = velo} = do
   let ipos = LU.interpolateFrame frac pos velo
   G.renderTexturedQuad P.playerSize ipos $ playerTextureId res

render _
       RenderState {ressources = res}
       E.Star {E.starPosition = pos, E.starCollected = False} =
   G.renderTexturedQuad S.starSize pos $ starTextureId res

render E.ActiveLayerScope
       RenderState {nextFrameFraction = frac}
       E.Platform {E.platformPosition = posOrAnim, E.platformBound = bound} = do
   G.withPushedMatrix $ do
      GL.glTranslatef <<< interpolatePlaformPos frac posOrAnim
      GL.glColor3f <<<* (0.7,0.7,0.7) >> G.drawBox bound
      GL.glLineWidth 4
      G.withPolyMode GL.gl_LINE $ GL.glColor3f <<<* (0.4,0.4,0.4) >> G.drawBox bound

render E.InactiveLayerScope
       RenderState {nextFrameFraction = frac}
       E.Platform {E.platformPosition = posOrAnim, E.platformBound = bound} = do
   G.withPushedMatrix $ do
      GL.glTranslatef <<< interpolatePlaformPos frac posOrAnim
      G.withBlend GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA $ do
         GL.glColor4f <<<<* (0.0,0.0,0.1,0.2) >> G.drawBox bound
         G.withPolyMode GL.gl_LINE $ GL.glColor4f <<<<* (0.0,0.0,0.3,0.2) >> G.drawBox bound

render _ _ _ = return ()


renderBound :: E.Bound -> IO ()
renderBound bound = do
   GL.glColor3f <<<* (0,0,0)
   GL.glLineWidth 1
   G.withPolyMode GL.gl_LINE $ G.drawBoxTree bound
