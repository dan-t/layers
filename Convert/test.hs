
import qualified FileData.Data2 as FD
import qualified GameData.Data as GD
import qualified Convert.ToGameData as TGD

toGameData :: FilePath -> IO GD.GameData
toGameData fp = do
   str <- readFile fp
   let fd = read str :: FD.Data
   return $ TGD.toGameData fd
