
module Data1ToData2 where
import qualified FileData.Data1 as D1
import qualified FileData.Data2 as D2

convertFile :: FilePath -> FilePath -> IO ()
convertFile d1Path d2Path = do
   d1File <- readFile d1Path
   let d1 = show . convert . read $ d1File
   writeFile d2Path d1

convert :: D1.Data -> D2.Data
convert data1 = 
   D2.Data D2.dataVersion (map convertLevel $ D1.levels data1)

convertLevel :: D1.Level -> D2.Level
convertLevel level = D2.Level entities layers
   where
      levelId  = D1.levelId level
      entities = player : stars
         where
            player = convertPlayer $ D1.player level
            stars  = map convertStar (D1.stars level)

      layers = [convertLayer $ D1.activeLayer level,
                convertLayer $ D1.otherLayer  level]


convertPlayer (D1.Player id initPos) = D2.Player id initPos
convertStar (D1.Star id pos) = D2.Star id pos

convertLayer (D1.Layer id gravity platforms) =
   D2.Layer (map convertPlatform platforms) gravity

convertPlatform (D1.Platform id size pos) =
   D2.Platform id (convertPos pos) size
   where
      convertPos (Right (D1.Moving velo path biDir)) =
         Right $ D2.Animation (D1.length velo) path biDir 

      convertPos (Left pos) = Left pos
