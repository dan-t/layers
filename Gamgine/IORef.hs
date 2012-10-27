
module Gamgine.IORef where
#include "Gamgine/Utils.cpp"
import Control.Applicative ((<$>))
import Control.Monad (void)
import qualified Control.Monad.State as ST
import qualified Data.IORef as R
IMPORT_LENS

type StateIORef a = ST.StateT (R.IORef a) IO 

-- | map a function on the value of the IORef
mapIORef :: (a -> b) -> R.IORef a -> IO b
mapIORef f ref = f <$> R.readIORef ref


{- functions to operate on the value of a IORef inside of a State -}

-- | get the value of the IORef inside of the State
get :: StateIORef a a
get = do
   ref <- ST.get
   ST.liftIO $ R.readIORef ref

-- | apply a function on the value of the IORef
gets :: (a -> b) -> StateIORef a b
gets f = do
   ref <- ST.get
   ST.liftIO $ mapIORef f ref

-- | apply the getter lens on the value of the IORef inside of the State
getsL :: LE.Lens a b -> StateIORef a b
getsL lens = do
   ref <- ST.get
   ST.liftIO $ getL ref lens

-- | put a value 
putL :: LE.Lens a b -> b -> StateIORef a ()
putL lens value = do
   ref <- ST.get
   void $ ST.liftIO $ setL ref lens value

-- | modify the value of the IORef inside of the State with a lens
modify :: (a -> a) -> StateIORef a ()
modify f = do
   ref <- ST.get
   ST.liftIO $ R.modifyIORef ref f

-- | modify the value of the IORef inside of the State with a lens
modifyL :: LE.Lens a b -> (b -> b) -> StateIORef a ()
modifyL lens f = do
   ref <- ST.get
   ST.liftIO $ modL ref lens f


{- functions to apply a lens to the value of a IORef -}

-- | apply the getter of the lens on the value of the IORef  
getL :: R.IORef a -> LE.Lens a b -> IO b
getL ref lens = mapIORef (LE.getL lens) ref

-- | apply the setter of the lens on the value of the IORef  
setL :: R.IORef a -> LE.Lens a b -> b -> IO ()
setL ref lens value = R.modifyIORef ref $ (LE.setL lens value)

-- | modify the value of the IORef with a lens
modL :: R.IORef a -> LE.Lens a b -> (b -> b) -> IO ()
modL ref lens f = R.modifyIORef ref $ LE.modL lens f
