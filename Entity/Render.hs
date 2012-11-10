
module Entity.Render where
import Control.Monad (when, forM_)
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Gamgine.Control ((?))
import qualified Gamgine.Gfx as G
import qualified Gamgine.Math.Box as B
import Gamgine.Math.Vect as V
import Gamgine.Gfx ((<<*), (<<<*), (<<<<*), (<<<))
import qualified GameData.Entity as E
import qualified GameData.Player as P
import qualified GameData.Star as S
import qualified GameData.Animation as A
import qualified Rendering.Ressources as RR


interpolateFrame :: Double -> V.Vect -> V.Vect -> V.Vect
interpolateFrame nextFrameFraction position velocity =
   position + (velocity * (V.v3 nextFrameFraction nextFrameFraction nextFrameFraction))


interpolatePlaformPos :: Double -> E.PositionOrAnimation -> V.Vect
interpolatePlaformPos _ (Left pos) = pos

interpolatePlaformPos nextFrameFraction (Right ani) =
   interpolateFrame nextFrameFraction (A.currentPosition ani) (A.currentVelocity ani)


render :: E.Scope ->
          RR.RenderState ->
          E.Entity ->
          IO ()

render _
       RR.RenderState {RR.nextFrameFraction = frac, RR.ressources = res}
       E.Player {E.playerPosition = pos, E.playerVelocity = velo@(vx:._),
                 E.playerOnBottom = onBot, E.playerWalkCycle = (_, angle)} =
   renderWalk P.playerSize pos (onBot && vx /= 0 ? angle $ 0) (RR.playerTextureId res)
   where
      renderWalk :: (Double,Double) -> Vect -> Double -> GL.GLuint -> IO ()
      renderWalk (sizeX, sizeY) translation angle texture =
         G.withPushedMatrix $ do
            GL.glTranslatef <<<* (sizeX * 0.5, sizeY * 0.5, 0)
            GL.glTranslatef <<< translation
            GL.glRotatef <<<<* (angle, 0, 0, -1)
            G.withTexture2d texture $
               G.withBlend GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA $
                  G.withPrimitive GL.gl_QUADS $ do
                     let coords   = G.quadTexCoords 1 1
                         vertices = G.quad (sizeX * (-0.5), sizeY * (-0.5)) (sizeX * 0.5, sizeY * 0.5)
                     GL.glColor3f <<<* (1,1,1)
                     forM_ (zip coords vertices) (\(c,v) -> do
                        GL.glTexCoord2f <<* c
                        GL.glVertex2f <<* v)

render _
       RR.RenderState {RR.ressources = res}
       E.Star {E.starPosition = pos, E.starCollected = False} =
   G.withPushedMatrix $ do
      GL.glTranslatef <<< pos
      G.renderTexturedQuad S.starSize $ RR.starTextureId res

render E.ActiveLayerScope
       RR.RenderState {RR.nextFrameFraction = frac}
       E.Platform {E.platformPosition = posOrAnim, E.platformBound = bound} = do
   G.withPushedMatrix $ do
      GL.glTranslatef <<< interpolatePlaformPos frac posOrAnim
      GL.glColor3f <<<* (0.7,0.7,0.7) >> G.drawBox bound
      GL.glLineWidth 4
      G.withPolyMode GL.gl_LINE $ GL.glColor3f <<<* (0.4,0.4,0.4) >> G.drawBox bound

render E.InactiveLayerScope
       RR.RenderState {RR.nextFrameFraction = frac}
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
