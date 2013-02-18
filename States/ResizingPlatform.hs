{-# LANGUAGE TupleSections #-}

module States.ResizingPlatform where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import Data.Composition ((.:))
import Gamgine.Control ((?))
import qualified Gamgine.Math.Vect as V
import qualified Gamgine.Math.Box as B
import qualified Gamgine.State.State as ST
import qualified GameData.Level as LV
import qualified GameData.Data as GD
import qualified GameData.Entity as E
import qualified GameData.Platform as PF
import qualified Entity.Id as EI
import qualified Entity.Position as EP
import qualified States.GameRunning as GR
import qualified States.CreatingPlatform as CP
IMPORT_LENS_AS_LE


data ResizingPlatform = ResizingPlatform {
   entityId :: Maybe Int,
   minPt    :: V.Vect,
   maxPt    :: V.Vect,
   basePos  :: V.Vect
   }

-- | the state for resizing a platform during edit mode
mkResizingPlatformState :: ST.State GD.Data
mkResizingPlatformState =
   mkState $ ResizingPlatform Nothing V.nullVec V.nullVec V.nullVec
   where
      mkState rp = ST.State {
         ST.enter = \mp gd ->
            case LV.findEntityAt mp $ LE.getL GD.currentLevelL gd of
                 Just e@E.Platform {} ->
                    let pos   = EP.position e
                        bound = E.platformBound e
                        minPt = pos + B.minPt bound
                        maxPt = pos + B.maxPt bound
                        in Just (gd, mkState (rp {entityId = Just $ EI.entityId e,
                                                  minPt    = minPt,
                                                  maxPt    = maxPt,
                                                  basePos  = mp}))
                 _ -> Nothing,

         ST.leave      = (, mkState (rp {entityId = Nothing, minPt = V.nullVec, maxPt = V.nullVec, basePos = V.nullVec})),
         ST.update     = (, mkState rp) . GR.update,
         ST.render     = ((, mkState rp) <$>) .: GR.render,
         ST.keyEvent   = (, mkState rp) .: flip const,
         ST.mouseEvent = (, mkState rp) .: flip const,

         ST.mouseMoved = \mp gd ->
            case rp of
                 ResizingPlatform {entityId = Just id, minPt = minPt, maxPt = maxPt, basePos = bp} ->
                    let diffVec = mp - bp
                        in (E.eMap (\e -> id == EI.entityId e ? CP.updatePosAndBound minPt (maxPt + diffVec) e $ e) gd,
                            mkState rp)

                 _ -> (gd, mkState rp)
         }
