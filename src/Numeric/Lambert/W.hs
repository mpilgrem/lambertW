module Numeric.Lambert.W
  ( lambertW0
  , safeLambertW0
  ) where

import           Data.Maybe ( fromMaybe )
import           Prelude hiding ( iterate )

-- | The principal branch (W0) of the Lambert W function. Stops execution and
-- displays an error message if the argument is less than -1/e.
lambertW0 :: Double -> Double
lambertW0 x = fromMaybe
  (error $ "lambertW0 : invalid argument, " <> show x)
  (safeLambertW0 x)

-- | The principal branch (W0) of the Lambert W function. Yields 'Nothing' if
-- the argument is less than -1/e.
safeLambertW0 :: Double -> Maybe Double
safeLambertW0 x
  | ex < (-1.0) = Nothing
  | ex == (-1.0) = Just (-1.0)
  | x == 0.0 = Just 0.0
  | otherwise = Just (iterate w0)
 where
  e = exp 1.0
  ex = e * x

  -- The argument cannot be 0.0 or (-1.0) and x cannot be 0.0.
  iterate :: Double -> Double
  iterate w =
    let w' = w * (1.0 + log (x / w)) / (1.0 + w)
    in  if (w > 0.0 && w' > w) || (w < 0.0 && w' < w)
          then iterate w'
          else w

  -- e * x cannot be <= (-1.0).
  w0 :: Double
  w0
    | x < 0.0 =
        let oneplusroot = 1.0 + sqrt (1.0 + ex)
        in  ex * log oneplusroot / (ex + oneplusroot)
    | x < e = 2.0 * x / (x + e)
    | otherwise =
        let logx = log x
        in  logx - log logx
