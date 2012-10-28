
module Callback.Key where
#include "Gamgine/Utils.cpp"
import qualified Data.List as L
import qualified Graphics.UI.GLFW as GLFW
import Gamgine.Math.Vect as V
import Gamgine.IORef as GR
import Gamgine.Utils as GU
import qualified AppData as AP
import qualified GameData.Entity as E
import qualified GameData.Player as PL
import qualified GameData.Layer as LY
import qualified GameData.Level as LV
IMPORT_LENS

type Pressed     = Bool
type KeyCallback = (GLFW.Key -> Pressed -> IO ())

newKeyCallback :: AP.AppDataRef -> AP.AppMode -> KeyCallback
newKeyCallback appDataRef _ = callback
   where
      callback GLFW.KeyLeft True   = accelerateToTheLeft

      callback GLFW.KeyLeft False  = accelerateToTheRight

      callback GLFW.KeyRight True  = accelerateToTheRight

      callback GLFW.KeyRight False = accelerateToTheLeft

      callback GLFW.KeyUp True     = jump

      callback GLFW.KeyTab True    = switchToNextLayer

      callback _ _                 = return ()


      switchToNextLayer = modL AP.currentLevelL LV.switchToNextLayer

      jump = updateEntity (GU.applyIf (\e -> E.isPlayer e && E.playerOnBottom e) $ \e ->
         let (vx:.vy:.vz:.()) = E.playerVelocity e
             in e {E.playerVelocity = V.v3 vx PL.jumpAcceleration vz,
                   E.playerOnBottom = False})

      accelerateToTheLeft  = updatePlayerVelocity ((+) $ V.v3 (-PL.playerVelocity) 0 0)
      accelerateToTheRight = updatePlayerVelocity ((+) $ V.v3 PL.playerVelocity 0 0)

      updatePlayerVelocity f = updateEntity (GU.applyIf E.isPlayer $ LE.modL E.playerVelocityL f)

      updateEntity f = modL AP.currentLevelL $ E.eMap f
      modL           = GR.modL appDataRef
      getL           = GR.getL appDataRef
