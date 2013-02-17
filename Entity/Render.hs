
module Entity.Render where
import Control.Monad (when, forM_)
import qualified Graphics.Rendering.OpenGL.Raw as GL
import Gamgine.Control ((?))
import qualified Gamgine.Gfx as G
import qualified Gamgine.Math.Box as B
import Gamgine.Math.Vect as V
import Gamgine.Gfx ((<<*), (<<<*), (<<<<*), (<<<))
import qualified Gamgine.State.RenderState as RS
import qualified GameData.Entity as E
import qualified GameData.Player as P
import qualified GameData.Enemy as EN
import qualified GameData.Star as S
import qualified GameData.Animation as A
import qualified Entity.Position as EP
import qualified Entity.Velocity as EV
import qualified Rendering.Ressources as RR


interpolateFrame :: Double -> V.Vect -> V.Vect -> V.Vect
interpolateFrame nextFrameFraction position velocity =
   position + (velocity * (V.v3 nextFrameFraction nextFrameFraction nextFrameFraction))


interpolateAnimationPos :: Double -> E.PositionOrAnimation -> V.Vect
interpolateAnimationPos _ (Left pos) = pos

interpolateAnimationPos nextFrameFraction (Right ani) =
   interpolateFrame nextFrameFraction (A.currentPosition ani) (A.currentVelocity ani)


render :: E.Scope ->
          RS.RenderState ->
          E.Entity ->
          IO ()

render _
       RS.RenderState {RS.nextFrameFraction = frac, RS.ressources = res}
       E.Player {E.playerPosition = pos, E.playerVelocity = velo@(vx:._),
                 E.playerOnBottom = onBot, E.playerWalkCycle = (_, angle)} =
   renderWalk P.playerSize pos (onBot && vx /= 0 ? angle $ 0) (RR.textureId RR.Player res)

render _
       RS.RenderState {RS.nextFrameFraction = frac, RS.ressources = res}
       e@E.Enemy {E.enemyPosition = pos, E.enemyLiving = True, E.enemyWalkCycle = (_, angle)} = do
   let (vx:._) = EV.velocity e
   renderWalk EN.enemySize (interpolateAnimationPos frac pos) (vx /= 0 ? angle $ 0) (RR.textureId RR.Enemy res)

render _
       RS.RenderState {RS.ressources = res}
       E.Star {E.starPosition = pos, E.starCollected = False} =
   G.withPushedMatrix $ do
      GL.glTranslatef <<< pos
      G.renderTexturedQuad S.starSize $ RR.textureId RR.Star res

render E.ActiveLayerScope
       RS.RenderState {RS.nextFrameFraction = frac}
       E.Platform {E.platformPosition = posOrAnim, E.platformBound = bound} = do
   G.withPushedMatrix $ do
      GL.glTranslatef <<< interpolateAnimationPos frac posOrAnim
      GL.glColor3f <<<* (0.7,0.7,0.7) >> G.drawBox bound
      GL.glLineWidth 4
      G.withPolyMode GL.gl_LINE $ GL.glColor3f <<<* (0.4,0.4,0.4) >> G.drawBox bound

render E.InactiveLayerScope
       RS.RenderState {RS.nextFrameFraction = frac}
       E.Platform {E.platformPosition = posOrAnim, E.platformBound = bound} = do
   G.withPushedMatrix $ do
      GL.glTranslatef <<< interpolateAnimationPos frac posOrAnim
      G.withBlend GL.gl_SRC_ALPHA GL.gl_ONE_MINUS_SRC_ALPHA $ do
         GL.glColor4f <<<<* (0.0,0.0,0.1,0.2) >> G.drawBox bound
         G.withPolyMode GL.gl_LINE $ GL.glColor4f <<<<* (0.0,0.0,0.3,0.2) >> G.drawBox bound

render _ _ _ = return ()


renderBound :: E.Bound -> IO ()
renderBound bound = do
   GL.glColor3f <<<* (0,0,0)
   GL.glLineWidth 1
   G.withPolyMode GL.gl_LINE $ G.drawBoxTree bound


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
