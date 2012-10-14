
module Renderer where
import Control.Monad (forM_)
import Graphics.Rendering.OpenGL.Raw
import Gamgine.Gfx
import qualified Gamgine.Coroutine as CO
import Gamgine.Math.Vect
import qualified GameData.Star as S
import qualified Entity.Render as ER

type Finished = Bool
-- | a render routine which is called/used until it returns Finished=True,
--   used for temporary animations
type Renderer = CO.CoroutineM IO ER.RenderState Finished

runRenderer = CO.runCoroutineM

finishRenderer :: (Bool, Renderer)
finishRenderer = (True, CO.CoroutineM $ f)
   where f state = return (True, CO.CoroutineM $ f)

continueRenderer f = (False, CO.CoroutineM $ f)

mkRenderer rd = CO.CoroutineM rd


fadeOutStar :: Vect -> Double -> ER.RenderState -> IO (Finished, Renderer)
fadeOutStar pos factor rstate = do
   let texId   = ER.starTextureId . ER.ressources $ rstate
       (sx,sy) = S.starSize
       f'      = factor + 0.02
   if f' < 1
      then do
         renderStar pos (sx+f',sy+f') (1,1,1,1-f') texId
         return $ continueRenderer $ fadeOutStar pos f'

      else return finishRenderer

   where
      renderStar :: Vect -> (Double,Double) -> RGBA -> GLuint -> IO ()
      renderStar (px:.py:._) (sx,sy) color texId = do
         let (minX, minY) = (px, py)
             (maxX, maxY) = (px + sx, py + sy)
         withTexture2d texId $
            withBlend gl_SRC_ALPHA gl_ONE_MINUS_SRC_ALPHA $
               withPrimitive gl_QUADS $ do
                  let coords   = quadTexCoords 1 1
                      vertices = quad (minX,minY) (maxX,maxY)
                  glColor4f <<<<* color
                  forM_ (zip coords vertices) (\(c,v) -> do
                     glTexCoord2f <<* c
                     glVertex2f <<* v)
