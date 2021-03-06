{-# LANGUAGE TupleSections #-}

module States.DefiningAnimation where
#include "Utils.cpp"
import qualified Graphics.UI.GLFW as GLFW
import Control.Applicative ((<$>))
import Data.Composition ((.:))
import Data.Maybe (fromJust)
import qualified Data.List as L
import qualified Gamgine.Math.Vect as V
import Gamgine.Control ((?))
import qualified Graphics.GL as GL
import qualified Gamgine.Gfx as Gfx
import Gamgine.Gfx ((<<<))
import qualified Gamgine.State.State as ST
import qualified Gamgine.State.MouseInfo as MI
import qualified Gamgine.State.InputInfo as II
import qualified States.GameRunning as GR
import qualified GameData.Level as LV
import qualified GameData.Entity as E
import qualified GameData.Enemy as EN
import qualified GameData.Data as GD
import qualified GameData.Animation as A
import qualified GameData.Platform as PF
import qualified Entity.Id as EI
import qualified Entity.Position as EP
IMPORT_LENS_AS_LE

data DefiningAnimation = DefiningAnimation {
   entityId :: Maybe Int,
   velocity :: Double,
   mousePos :: V.Vect,
   path     :: [V.Vect]
   }

-- | the state for defining an animation for e.g. a platform during edit mode
mkDefiningAnimationState :: ST.State GD.Data
mkDefiningAnimationState =
   mkState $ DefiningAnimation Nothing 0 V.nullVec []
   where
      mkState da = ST.State {
         ST.enter = \mp gd ->
            case LV.findEntityAt mp $ LE.getL GD.currentLevelL gd of
                 Just e | (not $ E.isPlatform e) && (not $ E.isEnemy e) -> Nothing
                        | otherwise ->
                    let id  = EI.entityId e
                        pos = EP.position e
                        gd' = E.eMap (\e -> id == EI.entityId e ? setPosition e (Left pos) $ e) gd
                        vel = getVelocity e
                        in Just (gd', mkState $ da {entityId = Just id, velocity = vel, mousePos = mp, path = [pos]})

                 _ -> Nothing,

         ST.leave = \gd ->
            let id  = fromJust $ entityId da
                pos | (L.length $ path da) > 1 = Right $ A.newAnimation (velocity da) (path da) True
                    | otherwise                = Left . L.head . path $ da
                gd' = E.eMap (\e -> id == EI.entityId e ? setPosition e pos $ e) gd
                in (gd', mkState $ da {entityId = Nothing, velocity = 0, mousePos = V.nullVec, path = []}),

         ST.update = (, mkState da) . GR.update,

         ST.render = \rs gd -> do
            gd' <- GR.render rs gd
            GL.glLineWidth 2
            GL.glColor3f <<< Gfx.rgb 0 0 0
            Gfx.draw GL.GL_LINE_STRIP (path da ++ [mousePos da])
            return $ (gd', mkState da),

         ST.keyEvent = (, mkState da) .: flip const,

         ST.mouseEvent = \MI.MouseInfo {MI.button = button, MI.status = status, MI.mousePos = mpos} gd ->
            case (button, status) of
                 (GLFW.MouseButton'1, II.Pressed) -> (gd, mkState $ da {mousePos = mpos, path = path da ++ [mpos]})
                 _                               -> (gd, mkState da),

         ST.mouseMoved = \mp gd -> (gd, mkState $ da {mousePos = mp})
         }


      setPosition p@E.Platform {} pos = p {E.platformPosition = pos}
      setPosition e@E.Enemy    {} pos = e {E.enemyPosition    = pos}
      setPosition e               _   = e

      getVelocity E.Platform {E.platformPosition = Right ani} = A.velocity ani
      getVelocity E.Platform {}                               = PF.platformVelocity
      getVelocity E.Enemy    {E.enemyPosition    = Right ani} = A.velocity ani
      getVelocity E.Enemy    {}                               = EN.enemyVelocity
