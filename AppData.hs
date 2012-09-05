
module AppData where
import qualified Data.IORef as R
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Lens.Strict as LE
import qualified Control.Monad.State as ST
import qualified Gamgine.Utils as GU
import qualified Background as BG
import qualified Boundary as BD
import qualified GameData.Data as GD
import qualified GameData.Level as LV
import qualified GameData.Layer as LY
import qualified Entity.Render as ER


data AppData = AppData {
   windowSize       :: (Int, Int),
   frustumSize      :: (Double, Double),
   background       :: BG.Background,
   boundary         :: BD.Boundary,
   renderRessources :: ER.Ressources,
   gameData         :: GD.Data,
   currentLevelId   :: Int,
   activeLayerId    :: Int
   }


-- | a lens for the current level
currentLevelLens = LE.lens getCurrentLevel setCurrentLevel
   where
      getCurrentLevel =
         (\AppData {gameData = gd, currentLevelId = curId} ->
            case L.find ((== curId) . LV.levelId) $ GD.levels gd of
                 Just level -> level
                 _          -> error $ "Couldn't find current level with id=" ++ show curId ++ "!")

      setCurrentLevel =
         (\level@LV.Level {LV.levelId = levId} appData@AppData {gameData = gameData} ->
            appData {currentLevelId = levId,
                     gameData = gameData {GD.levels = GU.replaceBy ((== levId) . LV.levelId) level $ GD.levels gameData}})


readCurrentLevel :: (LV.Level -> a) -> AppST a
readCurrentLevel f = do
   appDataRef <- ST.get
   appData    <- ST.liftIO $ R.readIORef appDataRef
   return (f $ LE.getL currentLevelLens appData)


modifyCurrentLevel :: (LV.Level -> LV.Level) -> AppST ()
modifyCurrentLevel f = modifyAppST $ LE.modL currentLevelLens f


currentLevel :: AppData -> LV.Level
currentLevel appData = LE.getL currentLevelLens appData


-- | a lens for the active layer
activeLayerLens = LE.lens getActiveLayer setActiveLayer
   where
      getActiveLayer =
         (\appData@AppData {activeLayerId = actId} ->
            let level = LE.getL currentLevelLens appData
                in case L.find ((== actId) . LY.layerId) $ LV.layers level of
                        Just layer -> layer
                        _          -> error $ "Couldn't find active layer with id=" ++ show actId ++ "!")

      setActiveLayer =
         (\layer appData ->
            let level  = LE.getL currentLevelLens appData
                layId  = LY.layerId layer
                level' = level {LV.layers = GU.replaceBy ((== layId) . LY.layerId) layer $ LV.layers level}
                in LE.setL currentLevelLens level' appData {activeLayerId = layId})


readActiveLayer :: (LY.Layer -> a) -> AppST a
readActiveLayer f = do
   appDataRef <- ST.get
   appData    <- ST.liftIO $ R.readIORef appDataRef
   return (f $ LE.getL activeLayerLens appData)


modifyActiveLayer :: (LY.Layer -> LY.Layer) -> AppST ()
modifyActiveLayer f = modifyAppST $ LE.modL activeLayerLens f


activeAndInactiveLayers :: AppData -> (LY.Layer, [LY.Layer])
activeAndInactiveLayers appData@AppData {activeLayerId = actLayerId} =
   L.foldr
      (\layer (actLayer, inactLayers) ->
         if LY.layerId layer == actLayerId
            then (layer, inactLayers)
            else (actLayer, layer : inactLayers))
      (LY.empty, [])
      layers
   where
      layers       = LV.layers currentLevel
      currentLevel = LE.getL currentLevelLens appData


newAppData :: GD.Data -> AppData
newAppData gameData = AppData {
   windowSize       = (0,0),
   frustumSize      = (0,0),
   background       = BG.empty,
   boundary         = BD.newBoundary currLevel,
   renderRessources = ER.Ressources (-1) (-1),
   gameData         = gameData,
   currentLevelId   = LV.levelId currLevel,
   activeLayerId    = LY.layerId actLayer
   }
   where
      currLevel = GD.levels gameData !! 0
      actLayer  = LV.layers currLevel !! 0


type AppDataRef = R.IORef AppData
type AppST      = ST.StateT AppDataRef IO


readAppST :: (AppData -> a) -> AppST a
readAppST f = GU.readSTIORef f


modifyAppST :: (AppData -> AppData) -> AppST ()
modifyAppST f = GU.modifySTIORef f


runAppST :: AppST a -> AppDataRef -> IO (a, AppDataRef)
runAppST = ST.runStateT
