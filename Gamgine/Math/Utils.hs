
module Gamgine.Math.Utils where

clamp :: Double -> Double -> Double -> Double
clamp min max value
   | value < min = min
   | value > max = max
   | otherwise = value

flipSign :: Double -> Double
flipSign num = -1 * signum num

maxFloat :: RealFloat a => a -> a
maxFloat a = encodeFloat m n
   where
      b = floatRadix a
      e = floatDigits a
      (_, e') = floatRange a
      m = b ^ e - 1
      n = e' - e

instance Bounded Double where
   maxBound = maxFloat (0 :: Double)
   minBound = -maxBound

minPositiveFloat :: RealFloat a => a -> a
minPositiveFloat a = encodeFloat 1 $ fst (floatRange a) - floatDigits a

minPositiveDouble = minPositiveFloat (0 :: Double)
