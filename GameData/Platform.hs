
module GameData.Platform where
import qualified Gamgine.Math.Box as B
import qualified GameData.Entity as E
import qualified Defaults as DF


newPlatform :: Int -> E.PositionOrAnimation -> B.Box -> E.Entity
newPlatform id posOrAnim bound = E.Platform {
   E.platformId       = id,
   E.platformPosition = posOrAnim,
   E.platformBound    = bound
   }


platformVelocity :: Double
platformVelocity = 2.5 / (fromIntegral DF.ticksPerSecond)
