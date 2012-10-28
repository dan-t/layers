module Gamgine.Utils where
#include "Gamgine/Utils.cpp"
import Prelude hiding (catch)
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Unsafe as BU
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch, SomeException)
import Data.Array.Storable
import Data.List
import Data.Bool.HT (if')
import Data.Bits ((.|.), shiftL)
import Data.Word
import Foreign.Ptr
import Gamgine.System
import Debug.Trace

-- | if a is just than apply f, otherwise 'return ()'
ifJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
ifJust (Just a) f = f a
ifJust _        _ = return ()

-- | apply f on a if p is true, otherwise just return a
applyIf :: (a -> Bool) -> (a -> a) -> a -> a
applyIf p f a | p a       = f a
              | otherwise = a

-- | apply f on a if p is true, otherwise 'return ()'
applyIfM :: Monad m => (a -> Bool) -> (a -> m ()) -> a -> m ()
applyIfM p f a | p a       = f a
               | otherwise = return ()

count :: Eq a => a -> [a] -> Int
count x ys = go x 0 ys
   where
      go x num []     = num
      go x num (y:ys) = go x (x == y ? num + 1 $ num) ys

-- | replace all entries in 'as' by 'new' for which 'f' returns true
replaceBy :: (a -> Bool) -> a -> [a] -> [a]
replaceBy f new as = map (\a -> if f a then new else a) as


errorsToStderr :: IO () -> IO ()
errorsToStderr action =
   catch action (\e -> do pn <- normalizedProgName
			  hPutStrLn stderr ("\n" ++ pn ++ ": " ++ show (e :: SomeException)))

showValue :: Show a => String -> a -> String
showValue name value = name ++ ": " ++ (show value) ++ "\n"

sv :: Show a => String -> a -> String
sv = showValue

infixl 1 ?
(?) :: Bool -> a -> a -> a
(?) = if'

for_ :: [a] -> (a -> b) -> [b]
for_ as f = map f as

maybe_ :: Maybe a -> b -> (a -> b) -> b
maybe_ m dflt f = maybe dflt f m

-- both lists have to be sorted ascending
firstFreeId :: Eq a => [a] -> [a] -> a
firstFreeId usedIds allIds = go usedIds allIds
   where
      go      _ []     = ERROR "Ups, all ids used!"
      go     [] (a:as) = a
      go (u:us) (a:as) = u /= a ? a $ go us as


word :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
word a b c d =  (fromIntegral a `shiftL` 24)
                .|. (fromIntegral b `shiftL` 16)
                .|. (fromIntegral c `shiftL`  8)
                .|. (fromIntegral d            )


bytesFromPointer :: Int -> Ptr Word8 -> IO BL.ByteString
bytesFromPointer n ptr = do
    s <- BU.unsafePackCStringLen (castPtr ptr, n)
    return $! BL.fromChunks [s]


bytesFromStorableArray :: Int -> StorableArray (Int, Int) Word8 -> IO BL.ByteString
bytesFromStorableArray n array = do
   bytes <- withStorableArray array (bytesFromPointer n)
   return bytes
