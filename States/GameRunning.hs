
module States.GameRunning where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import Data.Foldable (foldrM)
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
import qualified Updater as UP
import qualified States.State as ST
import qualified States.KeyInfo as KI
import qualified States.MouseInfo as MI
import qualified Level.Update as LU
import qualified Level.Render as LR
import qualified Convert.ToFileData as TF
import qualified Convert.ToGameData as TG
IMPORT_LENS


-- | the default input handling state when the
--   game is normally running a level
mkGameRunningState :: ST.State GD.Data
mkGameRunningState = ST.State {
   ST.enter      = \_ gd -> (gd, mkGameRunningState),
   ST.leave      = \gd -> (gd, mkGameRunningState),
   ST.update     = \gd -> (update gd, mkGameRunningState),

   ST.render     = \rs gd -> do
      gd' <- render rs gd
      return (gd', mkGameRunningState),

   ST.keyEvent   = \ki gd -> (keyEvent ki gd, mkGameRunningState),
   ST.mouseEvent = \_ gd -> (gd, mkGameRunningState),
   ST.mouseMoved = \_ gd -> (gd, mkGameRunningState)
   }


update :: GD.Data -> GD.Data
update gd = EV.handleEvents events (LE.setL GD.currentLevelL level' gd)
   where
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

        (GLFW.CharKey 'P', KI.Pressed) ->
           let starId  = LV.freeEntityId $ LE.getL GD.currentLevelL gd
               starPos = V.v3 (mpx - (fst S.starSize * 0.5)) (mpy - (snd S.starSize * 0.5)) 0
               in LE.modL (LV.entitiesL . GD.currentLevelL) (S.newStar starId starPos :) gd

        (GLFW.CharKey 'R', KI.Pressed) ->
           case LV.findEntityAt mp $ LE.getL GD.currentLevelL gd of
                Just e -> E.eFilter ((/= EI.entityId e) . EI.entityId) gd
                _      -> gd

        (GLFW.CharKey 'A', KI.Pressed) ->
           GD.addEmptyLevel gd

        (GLFW.CharKey 'N', KI.Pressed)
           | L.any (== KI.Shift) (KI.modifiers ki) -> GD.toPreviousLevel gd
           | otherwise                             -> GD.toNextLevel gd

        (GLFW.CharKey 'L', KI.Pressed) ->
           LE.modL GD.currentLevelL (TG.toLevel . TF.toLevel) gd

        _ -> gd
           
   where
      toTheLeft  = V.v3 (-PL.playerVelocity) 0 0
      toTheRight = V.v3 PL.playerVelocity 0 0  
