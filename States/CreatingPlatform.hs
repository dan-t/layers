{-# LANGUAGE TupleSections #-}

module States.CreatingPlatform where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import Data.Composition ((.:))
import Gamgine.Control ((?))
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified GameData.Level as LV
import qualified GameData.Data as GD
import qualified GameData.Entity as E
import qualified GameData.Platform as PF
import qualified Entity.Id as EI
import qualified States.State as ST
import qualified States.GameRunning as GR
IMPORT_LENS


data CreatingPlatform = CreatingPlatform {
   entityId :: Maybe Int,
   startPos :: V.Vect
   }

-- | the state for creating a platform during edit mode
mkCreatingPlatformState :: ST.State GD.Data
mkCreatingPlatformState =
   mkState $ CreatingPlatform Nothing V.nullVec
   where
      mkState cp = ST.State {
         ST.enter = \mp gd ->
            let id    = LV.freeEntityId . LE.getL GD.currentLevelL $ gd
                bound = B.Box V.nullVec V.nullVec
                gd'   = LE.modL GD.currentLevelL (LV.addEntity (PF.newPlatform id (Left mp) bound) LV.ToActiveLayer) gd
                in (gd', mkState cp {entityId = Just id, startPos = mp}),

         ST.leave      = (, mkState cp {entityId = Nothing, startPos = V.nullVec}),
         ST.update     = (, mkState cp) . GR.update,
         ST.render     = ((, mkState cp) <$>) .: GR.render,
         ST.keyEvent   = (, mkState cp) .: flip const,
         ST.mouseEvent = (, mkState cp) .: flip const,

         ST.mouseMoved = \mp gd ->
            case cp of
                 CreatingPlatform {entityId = Just id, startPos = sp} ->
                    (E.eMap (\e -> id == EI.entityId e ? updatePosAndBound mp sp e $ e) gd,
                     mkState cp)
         }


updatePosAndBound :: V.Vect -> V.Vect -> E.Entity -> E.Entity
updatePosAndBound v1 v2 pf@(E.Platform {}) =
   let minPt   = V.minVec v1 v2
       maxPt   = V.maxVec v1 v2
       diffVec = V.map abs $ maxPt - minPt
       in pf {E.platformPosition = Left minPt,
              E.platformBound    = B.Box V.nullVec diffVec}

updatePosAndBound _ _ e = e
