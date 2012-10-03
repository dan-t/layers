
module Utils where
import qualified Gamgine.Math.Vect as V

interpolateFrame :: Double -> V.Vect -> V.Vect -> V.Vect
interpolateFrame nextFrameFraction position velocity =
   position + (velocity * (V.v3 nextFrameFraction nextFrameFraction nextFrameFraction))
