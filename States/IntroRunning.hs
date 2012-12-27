{-# LANGUAGE TupleSections #-}

module States.IntroRunning where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import Data.Composition ((.:))
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as G
import Gamgine.Gfx ((<<<*))
import qualified States.State as ST
import qualified States.KeyInfo as KI
import qualified States.MouseInfo as MI
import qualified Rendering.Ressources as RR
import qualified GameData.Data as GD
import qualified GLF
IMPORT_LENS


-- | the intro state after starting the game
mkIntroRunningState :: ST.State GD.Data
mkIntroRunningState = ST.State {
   ST.enter      = (Just . (, mkIntroRunningState)) .: flip const,
   ST.leave      = (, mkIntroRunningState),
   ST.update     = (, mkIntroRunningState),
   ST.render     = ((, mkIntroRunningState) <$>) .: render,
   ST.keyEvent   = (, mkIntroRunningState) .: flip const,
   ST.mouseEvent = (, mkIntroRunningState) .: flip const,
   ST.mouseMoved = (, mkIntroRunningState) .: flip const
   }


render :: RR.RenderState -> a -> IO a
render RR.RenderState {RR.ressources = res, RR.frustumSize = (fx, fy)} gd = do
   G.withPushedMatrix $ do
      GL.glMatrixMode GL.gl_MODELVIEW
      GL.glLoadIdentity

      G.withPushedMatrix $ do
         GLF.setCurrentFont $ RR.crystalFontId res
         GLF.Bounds (minx, miny) (maxx, maxy) <- GLF.getStringBounds introStr
         GL.glTranslatef <<<* (fx / 2 - ((minx + maxx) * 2) , fy / 2, 0)
         GL.glScalef <<<* (3.75,3,3)
         GLF.drawSolidString introStr

      G.withPushedMatrix $ do
         GLF.setCurrentFont $ RR.courierFontId res
         GLF.Bounds (minx, miny) (maxx, maxy) <- GLF.getStringBounds helpStr
         GL.glTranslatef <<<* (fx / 2 - ((minx + maxx) / 2) , fy * 0.1, 0)
         GL.glScalef <<<* (1,1,1)
         GLF.drawSolidString helpStr

   return gd
   where
      introStr = "LAYERS"
      helpStr  = "<Press Space>"
