
module Rendering.Renderer where
import Control.Monad (forM_)
import qualified Graphics.GL as GL
import qualified Gamgine.Gfx as G
import Gamgine.Gfx ((<<), (<<<<), (<<<))
import qualified Gamgine.Coroutine as CO
import Gamgine.Math.Vect
import qualified Gamgine.State.RenderState as RS
import qualified GameData.Star as S
import qualified Rendering.Ressources as RR

type Finished = Bool
-- | a render routine which is called/used until it returns Finished=True,
--   used for temporary animations
type Renderer = CO.CoroutineM IO RS.RenderState Finished

runRenderer = CO.runCoroutineM

finishRenderer :: (Bool, Renderer)
finishRenderer = (True, CO.CoroutineM $ f)
   where f state = return (True, CO.CoroutineM $ f)

continueRenderer f = (False, CO.CoroutineM $ f)

mkRenderer rd = CO.CoroutineM rd


fadeOutStar :: Vect -> Double -> RS.RenderState -> IO (Finished, Renderer)
fadeOutStar pos factor rstate = do
   let texId   = RR.textureId RR.Star $ RS.ressources rstate
       (sx,sy) = S.starSize
       f'      = factor + 0.02
   if f' < 1
      then do
         renderStar pos (sx+f',sy+f') (1,1,1,1-f') texId
         return $ continueRenderer $ fadeOutStar pos f'

      else return finishRenderer

   where
      renderStar :: Vect -> (Double,Double) -> G.RGBA -> GL.GLuint -> IO ()
      renderStar (px:.py:._) (sx,sy) color texId = do
         let (minX, minY) = (px, py)
             (maxX, maxY) = (px + sx, py + sy)
         G.withTexture2d texId $
            G.withBlend GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA $
               G.withPrimitive GL.GL_QUADS $ do
                  let coords   = G.quadTexCoords 1 1
                      vertices = G.quad (minX,minY) (maxX,maxY)
                  GL.glColor4f <<<< color
                  forM_ (zip coords vertices) (\(c,v) -> do
                     GL.glTexCoord2f << c
                     GL.glVertex2f << v)


fadeOutEnemy :: Vect -> (Double,Double) -> RS.RenderState -> IO (Finished, Renderer)
fadeOutEnemy pos (sizeX, sizeY) rstate = do
   let texId = RR.textureId RR.Enemy $ RS.ressources rstate
   G.withPushedMatrix $ do
      GL.glTranslatef <<< G.xyz (sizeX * 0.5) (sizeY * 0.5) 0
      GL.glTranslatef <<< pos
      G.withTexture2d texId $
         G.withBlend GL.GL_SRC_ALPHA GL.GL_ONE_MINUS_SRC_ALPHA $
            G.withPrimitive GL.GL_QUADS $ do
               let coords   = G.quadTexCoords 1 1
                   vertices = G.quad (sizeX * (-0.5), sizeY * (-0.5)) (sizeX * 0.5, sizeY * 0.5)
               GL.glColor3f <<< G.rgb 1 1 1
               forM_ (zip coords vertices) (\(c,v) -> do
                  GL.glTexCoord2f << c
                  GL.glVertex2f << v)

   let sizeY' = sizeY - 0.05
   if sizeY' > 0
      then return $ continueRenderer $ fadeOutEnemy pos (sizeX, sizeY')
      else return finishRenderer
