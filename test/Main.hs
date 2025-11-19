{-# LANGUAGE MagicHash #-}

module Main
  ( main
  ) where

import           Data.Bits ( (.&.) )
import           Data.Maybe ( isNothing )
import           GHC.Exts ( Double (..),  castDoubleToWord64# )
import           GHC.Word ( Word64 (..) )
import           Numeric.Lambert.W ( omega, lambertW0, safeLambertW0 )
import           Test.Tasty
import qualified Test.Tasty.QuickCheck as QC

prop_equality :: Word64 -> Double -> Bool
prop_equality prec x =
    let res = safeLambertW0 x
    in  if ex < (-1.0)
          then
            isNothing res
          else
            maybe False (\w -> eq prec (w * exp w) x) res
 where
  e = exp 1.0
  ex = e * x

prop_omega :: Word64 -> Bool
prop_omega prec = eq prec (omega * exp omega) 1.0

prop_lambertW0_1 :: Word64 -> Bool
prop_lambertW0_1 prec = eq prec omega (lambertW0 1.0)

eq :: Word64 -> Double -> Double -> Bool
eq maxUlps a b =
  let ia = conv $ doubleToWord64 a
      ib = conv $ doubleToWord64 b
      diff = if ia > ib then ia - ib else ib - ia
  in  diff <= maxUlps
 where
  conv :: Word64 -> Word64
  conv i = if i .&. mask /= 0 then mask - i else i + mask
   where
    mask = 0x8000000000000000

  doubleToWord64 :: Double -> Word64
  doubleToWord64 (D# d#) = W64# (castDoubleToWord64# d#)

main :: IO ()
main = defaultMain properties

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = localOption (QC.QuickCheckTests 1000000) $
  testGroup "(checked by QuickCheck)"
    [ localOption (QC.QuickCheckTests 1000000) $
        testPropertyWithPrec 14 "w * exp w == x" prop_equality
    , localOption (QC.QuickCheckTests 1) $
        testPropertyWithPrec 1 "Ω * exp Ω = 1" prop_omega
    , localOption (QC.QuickCheckTests 1) $
        testPropertyWithPrec 1 "Ω = W_0(1)" prop_lambertW0_1
    ]
 where
  testPropertyWithPrec ::
       QC.Testable a
    => Word64
       -- ^ The precision for the comparison, in ULPs (Unit in the Last Place).
    -> String
    -> (Word64 -> a)
    -> TestTree
  testPropertyWithPrec prec desc property =
    QC.testProperty (wPrec prec desc) (property prec)

  wPrec :: Word64 -> String -> String
  wPrec prec desc = desc <> " (precision: " <> show prec <> ")"
