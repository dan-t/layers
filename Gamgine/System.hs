{-# LANGUAGE ScopedTypeVariables #-}

module Gamgine.System where
import System.Environment (getProgName, getEnv)
import System.Directory (createDirectoryIfMissing, getAppUserDataDirectory, getDirectoryContents)
import Data.List (takeWhile)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)
import Control.Exception (try)
import Control.Monad (filterM)

normalizedProgName :: IO (String)
normalizedProgName = do
   pn <- getProgName
   return $ takeWhile (/= '.') pn

getCurrentDate :: IO (Integer,Int,Int) -- :: (year,month,day)
getCurrentDate = getCurrentTime >>= return . toGregorian . utctDay

getEnvOrDefault :: String -> String -> IO String
getEnvOrDefault envVar defaultValue = do
   result <- try $ getEnv envVar
   case result of
	Right value          -> return value
	Left  (_ :: IOError) -> return defaultValue

appDirectory = normalizedProgName >>= \pn -> getAppUserDataDirectory pn

getAndCreateAppDir :: IO (FilePath)
getAndCreateAppDir = do
   dir <- appDirectory
   createDirectoryIfMissing True dir
   return dir

getDirContents :: FilePath -> IO [FilePath]
getDirContents dir = do
   entries <- getDirectoryContents dir
   filterM notDots entries
   where
      notDots entry = return . not $ "." == entry || ".." == entry
