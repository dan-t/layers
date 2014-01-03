
module Ressources where

#ifdef CABAL
import qualified Paths_layers as PL
#endif

getDataFileName :: FilePath -> IO FilePath
getDataFileName path =
#ifdef CABAL
   PL.getDataFileName path
#else
   return $ srcDir ++ path
   where
      srcDir = "/home/dan/projekte/layers/src/"
#endif

getImageFilePath :: String -> IO FilePath
getImageFilePath imageName = getDataFileName $ imageDir ++ imageName
   where
      imageDir = "Ressources/Images/"


getFontFilePath :: String -> IO FilePath
getFontFilePath fontName = getDataFileName $ fontDir ++ fontName
   where
      fontDir = "Ressources/Fonts/"
