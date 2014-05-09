{-# LANGUAGE TupleSections #-}

module States.IntroRunning where
#include "Utils.cpp"
import Control.Applicative ((<$>))
import Data.Composition ((.:))
import qualified Graphics.Rendering.OpenGL.Raw as GL
import qualified Gamgine.Gfx as G
import Gamgine.Gfx ((<<<))
import qualified Gamgine.Font.GLF as GLF
import qualified Gamgine.State.RenderState as RS
import qualified Gamgine.State.State as ST
import qualified Gamgine.State.KeyInfo as KI
import qualified Gamgine.State.MouseInfo as MI
import qualified Rendering.Ressources as RR
import qualified GameData.Data as GD
IMPORT_LENS_AS_LE


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


render :: RS.RenderState -> a -> IO a
render RS.RenderState {RS.ressources = res, RS.frustumSize = (fx, fy)} gd = do
   G.withPushedMatrix $ do
      GL.glMatrixMode GL.gl_MODELVIEW
      GL.glLoadIdentity

      G.withPushedMatrix $ do
         GLF.setCurrentFont $ RR.fontId RR.Crystal res
         GLF.Bounds (minx, miny) (maxx, maxy) <- GLF.getStringBounds introStr
         GL.glTranslatef <<< G.xyz (fx / 2 - ((minx + maxx) * 2)) (fy / 2) 0
         GL.glScalef <<< G.xyz 3.75 3 3
         GLF.drawSolidString introStr

      G.withPushedMatrix $ do
         GLF.setCurrentFont $ RR.fontId RR.Courier res
         GLF.Bounds (minx, miny) (maxx, maxy) <- GLF.getStringBounds helpStr
         GL.glTranslatef <<< G.xyz (fx / 2 - ((minx + maxx) / 2)) (fy * 0.1) 0
         GL.glScalef <<< G.xyz 1 1 1
         GLF.drawSolidString helpStr

   return gd
   where
      introStr = "LAYERS"
      helpStr  = "<Press Space>"
