{-# LANGUAGE TupleSections #-}

module States.GameRunning where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import Data.Foldable (foldrM)
import Data.Composition ((.:))
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import qualified Data.List as L
import qualified GameData.Player as PL
import qualified GameData.Entity as E
import qualified GameData.Star as S
import qualified GameData.Level as LV
import qualified GameData.Data as GD
import qualified Entity.Id as EI
import qualified Rendering.Ressources as RR
import qualified Event as EV
import qualified States.State as ST
import qualified States.KeyInfo as KI
import qualified States.MouseInfo as MI
import qualified Level.Update as LU
import qualified Level.Render as LR
IMPORT_LENS


-- | the default input handling state when the
--   game is normally running a level
mkGameRunningState :: ST.State GD.Data
mkGameRunningState = ST.State {
   ST.enter      = (Just . (, mkGameRunningState)) .: flip const,
   ST.leave      = (, mkGameRunningState),
   ST.update     = (, mkGameRunningState) . update,
   ST.render     = ((, mkGameRunningState) <$>) .: render,
   ST.keyEvent   = (, mkGameRunningState) .: keyEvent,
   ST.mouseEvent = (, mkGameRunningState) .: flip const,
   ST.mouseMoved = (, mkGameRunningState) .: flip const
   }


update :: GD.Data -> GD.Data
update gd = LE.setL GD.currentLevelL level'' gd
   where
      level''          = EV.handleEvents events level'
      (events, level') = LU.update $ LE.getL GD.currentLevelL gd


render :: RR.RenderState -> GD.Data -> IO GD.Data
render rs gd = do
   level' <- LR.render rs $ LE.getL GD.currentLevelL gd
   return $ LE.setL GD.currentLevelL level' gd


keyEvent :: KI.KeyInfo -> GD.Data -> GD.Data
keyEvent ki@KI.KeyInfo {KI.key = key, KI.status = status, KI.mousePos = mp@(mpx:.mpy:.mpz:.())} gd =
   case (key, status) of
        (GLFW.KeyLeft, KI.Pressed) ->
           E.eMap (PL.accelerate toTheLeft) gd

        (GLFW.KeyLeft, KI.Released) ->
           E.eMap (PL.accelerate toTheRight) gd

        (GLFW.KeyRight, KI.Pressed) ->
           E.eMap (PL.accelerate toTheRight) gd

        (GLFW.KeyRight, KI.Released) ->
           E.eMap (PL.accelerate toTheLeft) gd

        (GLFW.KeyUp, KI.Pressed) ->
           E.eMap PL.jump gd

        (GLFW.KeySpace, KI.Pressed) ->
           E.eMap PL.jump gd

        (GLFW.CharKey ' ', KI.Pressed) ->
           E.eMap PL.jump gd

        (GLFW.KeyTab, KI.Pressed) ->
           LE.modL GD.currentLevelL LV.toNextLayer gd

        _ -> gd
           
   where
      toTheLeft  = V.v3 (-PL.playerVelocity) 0 0
      toTheRight = V.v3 PL.playerVelocity 0 0  
