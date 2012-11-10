{-# LANGUAGE TupleSections, MultiParamTypeClasses #-}

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

data GameRunning = GameRunning

-- | the default state for the running game
instance ST.State GameRunning GD.Data where
   update     gr gd     = (gr, update gd)
   render     gr gd rs  = (gr,) <$> render gd rs
   keyEvent   gr gd key = (gr, keyEvent gd key)


update :: GD.Data -> GD.Data
update gd = do
   let (events, level') = LU.update $ LE.getL GD.currentLevelL gd
   EV.handleEvents events (LE.setL GD.currentLevelL level' gd)


render :: GD.Data -> RR.RenderState -> IO GD.Data
render gd rs = do
   level' <- LR.render rs $ LE.getL GD.currentLevelL gd
   return $ LE.setL GD.currentLevelL level' gd


keyEvent :: GD.Data -> KI.KeyInfo -> GD.Data
keyEvent gd ki@KI.KeyInfo {KI.key = key, KI.status = status, KI.mousePos = mp@(mpx:.mpy:.mpz:.())} =
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
           | KI.withShift ki -> GD.toPreviousLevel gd
           | otherwise       -> GD.toNextLevel gd

        (GLFW.CharKey 'L', KI.Pressed) ->
           LE.modL GD.currentLevelL (TG.toLevel . TF.toLevel) gd
           
   where
      toTheLeft  = V.v3 (-PL.playerVelocity) 0 0
      toTheRight = V.v3 PL.playerVelocity 0 0  
