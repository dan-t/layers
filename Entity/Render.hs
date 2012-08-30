
module Entity.Render where
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Ressources as R
import qualified Gamgine.Gfx as G
import qualified GameData.Entity as E
import qualified GameData.Player as P
import qualified GameData.Star as S


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

render scope RenderState {ressources = res} E.Star {E.starPosition = pos} =
   G.renderTexturedQuad S.starSize pos $ starTextureId res

render _ _ _ = return ()
