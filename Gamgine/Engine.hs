
module Gamgine.Engine where
import Graphics.UI.GLFW
import Control.Monad.State (MonadIO, liftIO)

updateLoop :: (MonadIO m) => Double -> Int -> m a -> Double -> m (Double, Double)
updateLoop skipTicks maxFrameSkip update nextFrame = do
   loop skipTicks maxFrameSkip update nextFrame 0
   where
      loop :: (MonadIO m) => Double -> Int -> m a -> Double -> Int -> m (Double, Double)
      loop skipTicks maxFrameSkip update nextFrame skippedFrames = do
	 time <- liftIO getTime
	 if time > nextFrame && skippedFrames < maxFrameSkip
	    then do
	       update
	       loop skipTicks maxFrameSkip update (nextFrame + skipTicks) (skippedFrames + 1)
	    else do
	       interpol <- interpolation time nextFrame skipTicks
	       return (nextFrame,interpol)

      interpolation time nextFrame skipTicks = do
	 return $ (time - skipTicks - nextFrame) / skipTicks
