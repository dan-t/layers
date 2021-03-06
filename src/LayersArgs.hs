{-# LANGUAGE DeriveDataTypeable #-}

module LayersArgs where
import System.Console.CmdArgs
import System.IO.Unsafe (unsafePerformIO)
import qualified Ressources as RES

#ifdef CABAL
import Data.Version (showVersion)
import Paths_layers_game (version)
#endif

data LayersArgs = LayersArgs {
   editMode       :: Bool,
   loadLevelsFrom :: FilePath,
   saveLevelsTo   :: FilePath
   } deriving (Data, Typeable, Show, Eq)

initLayersArgs = LayersArgs {
   editMode       = False &= help "Start layers in edit mode",
   loadLevelsFrom = defaultLoadFrom &= help ("Load levels from file (default='" ++ defaultLoadFrom ++ "')") &= typFile,
   saveLevelsTo   = defaultSaveTo   &= help ("Save levels to file (default='" ++ defaultSaveTo ++ "')") &= typFile
   }
   &= program "layers"
   &= summary ""
   &= help "A prototypical 2d platform game."
   &= helpArg [explicit, name "help", name "h"]
   &= versionArg [explicit, name "version", name "v", summary versionInfo ]
   &= details detailsText

detailsText = [
   "Player Controls:",
   "  left/right arrow key = moving left/right",
   "  space/up arrow key   = jumping",
   "  tab key              = switch layer",
   "  q/escape key         = exit game",
   "",
   "Edit Mode:",
   "  a key = add new level after the current one and switch to it",
   "  A key = add new level before the current one and switch to it",
   "  n key = goto next level",
   "  N key = goto previous level",
   "  m key = move current level after the next one",
   "  M key = move current level before the previous one",
   "  l key = reload current level",
   "  s key = save levels to file specified by '-s'",
   "  r key = remove game object under mouse cursor",
   "  p key = place a star at mouse cursor position",
   "  e key = place an enemy at mouse cursor position",
   "  u key = start changing path of platform/enemy under mouse cursor,",
   "          define path points by moving the mouse and clicking",
   "          left mouse button, press 'u' again to end path definition",
   "",
   "  left mouse button + ctrl  = move game object under mouse cursor ",
   "                              (hold mouse button pressed)",
   "  left mouse button + shift = resize platform under mouse cursor ",
   "                              (hold mouse button pressed)",
   "  left mouse button         = define new platform by holding the",
   "                              button pressed and moving the mouse",
   "  mouse wheel               = change scale of orthographic projection"]

defaultSaveTo = "LayersData.hs"
defaultLoadFrom = unsafePerformIO $ RES.getDataFileName "Ressources/Levels.hs"

versionInfo :: String
versionInfo =
#ifdef CABAL
   "layers version " ++ showVersion version
#else
   "layers version unknown (not built with cabal)"
#endif

getLayersArgs :: IO LayersArgs
getLayersArgs = cmdArgs initLayersArgs
