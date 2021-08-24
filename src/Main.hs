module Main where

import           Definition
import qualified HillClimb       as HC
import qualified IterLocalSearch as ILS
import qualified PatternSearch   as PS
import           System.Random   (Random (random), StdGen, newStdGen)

f1 :: ObjFun
f1 [x, y] = sin (x + y) + (x - y) ** 2 - 1.5 * x + 2.5 * y + 1.0
f1 _      = error "wrong number of parameters"

ftest :: ObjFun
ftest [x] = x**2
ftest _   = error ""

main :: IO ()
main = do
  let p = PS.Params
        { PS.z = f1
        , PS.limits = inclusive [(-1.5, 4.0), (-3.0, 4.0)]
        , PS.select = minimize f1
        , PS.precision = [1e-4, 1e-4] }
  let s1 = PS.search p [1.0, 1.0] [0.0, 0.0]
  print (s1, f1 s1)

  let p = HC.Params
        { HC.z = f1
        , HC.limits = inclusive [(-1.5, 4.0), (-3.0, 4.0)]
        , HC.noise = [0.1, 0.1]
        , HC.tweakProb = 1.0
        , HC.tweakN = 5
        , HC.select = minimize f1
        , HC.stop = NoImprovements 10 1e-6 }
  rg2 <- newStdGen
  let s2 = HC.optimize p rg2
  print (s2, f1 s2)

  let p = ILS.Params
        { ILS.z              = f1
        , ILS.limits         = inclusive [(-1.5, 4.0), (-3.0, 4.0)]
        , ILS.perturbTries   = 5
        , ILS.noise          = [1.0, 1.0]
        , ILS.localStep      = [1.0, 1.0]
        , ILS.localPrecision = [1e-4, 1e-4]
        , ILS.select         = minimize f1
        , ILS.tolerance      = 0.5
        , ILS.stop           = NoImprovements 10 1e-6 }
  rg3 <- newStdGen
  let s3 = ILS.optimize p rg3
  print (s3, f1 s3)

