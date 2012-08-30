
module Entity.Render where
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Ressources as R
import qualified Gamgine.Gfx as G
import Gamgine.Gfx ((<<<*), (<<<<*), (<<<))
import qualified GameData.Entity as E
import qualified GameData.Player as P
import qualified GameData.Star as S
import qualified GameData.Animation as A


-- | render ressources of entities
data Ressources = Ressources {
   playerTextureId :: GL.GLuint,
   starTextureId   :: GL.GLuint
   } deriving Show


data RenderState = RenderState {
   frameInterpolation :: Double,
   ressources         :: Ressources
   } deriving Show


newRessources :: IO Ressources
newRessources = do
   playTex   <- R.getImageFilePath "Player.png"
   starTex   <- R.getImageFilePath "Star.png"
   playTexId <- G.makeTexture2d playTex GL.gl_REPEAT
   starTexId <- G.makeTexture2d starTex GL.gl_REPEAT
   return $ Ressources playTexId starTexId


render :: E.Scope -> RenderState -> E.Entity -> IO ()
render scope RenderState {ressources = res} E.Player {E.playerPosition = pos} =
   G.renderTexturedQuad P.playerSize pos $ playerTextureId res

render scope RenderState {ressources = res} E.Star {E.starPosition = pos, E.starCollected = False} =
   G.renderTexturedQuad S.starSize pos $ starTextureId res

render E.ActiveLayerScope   _ E.Platform {E.platformPosition = posOrAnim, E.platformBound = bound} = do
   G.withPushedMatrix $ do
      GL.glTranslatef <<< E.currentPosition posOrAnim
      GL.glColor3f <<<* (0.7,0.7,0.7) >> G.drawBox bound
      GL.glLineWidth 4
      G.withPolyMode GL.gl_LINE $ GL.glColor3f <<<* (0.4,0.4,0.4) >> G.drawBox bound

render E.InactiveLayerScope _ E.Platform {E.platformPosition = posOrAnim, E.platformBound = bound} = do
   G.withPushedMatrix $ do
      GL.glTranslatef <<< E.currentPosition posOrAnim
      G.withBlend GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA $ do
         GL.glColor4f <<<<* (0.0,0.0,0.1,0.2) >> G.drawBox bound
         G.withPolyMode GL.gl_LINE $ GL.glColor4f <<<<* (0.0,0.0,0.3,0.2) >> G.drawBox bound

render _ _ _ = return ()
