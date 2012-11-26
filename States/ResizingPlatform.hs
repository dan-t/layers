{-# LANGUAGE TupleSections #-}

module States.ResizingPlatform where
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
import qualified Entity.Position as EP
import qualified States.State as ST
import qualified States.GameRunning as GR
IMPORT_LENS


data ResizingPlatform = ResizingPlatform {
   entityId :: Maybe Int,
   startPos :: V.Vect,
   basePos  :: V.Vect
   }

-- | the state for resizing a platform during edit mode
mkResizingPlatformState :: ST.State GD.Data
mkResizingPlatformState =
   mkState $ ResizingPlatform Nothing V.nullVec V.nullVec
   where
      mkState rp = ST.State {
         ST.enter = \mp gd ->
            case LV.findEntityAt mp $ LE.getL GD.currentLevelL gd of
                 Just e@E.Platform {} -> Just (gd, mkState (rp {entityId = Just $ EI.entityId e,
                                                                startPos = mp,
                                                                basePos  = B.maxPt . E.platformBound $ e}))
                 _                    -> Nothing,

         ST.leave      = (, mkState (rp {entityId = Nothing, startPos = V.nullVec, basePos = V.nullVec})),
         ST.update     = (, mkState rp) . GR.update,
         ST.render     = ((, mkState rp) <$>) .: GR.render,
         ST.keyEvent   = (, mkState rp) .: flip const,
         ST.mouseEvent = (, mkState rp) .: flip const,

         ST.mouseMoved = \mp gd ->
            case rp of
                 ResizingPlatform {entityId = Just id, startPos = sp, basePos = bp} ->
                    let diffVec = mp - sp
                        in (E.eMap (\e -> id == EI.entityId e ? LE.modL E.platformBoundL (\pb -> pb {B.maxPt = bp + diffVec}) e $ e) gd,
                            mkState rp)

                 _ -> (gd, mkState rp)
         }
