module Main where

import           Definition
import qualified HillClimb     as HC
import qualified PatternSearch as PS
import           System.Random (Random (random), StdGen, newStdGen)

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
  print $ PS.search p [1.0, 1.0] [0.0, 0.0]

  let p = HC.Params
        { HC.z = f1
        , HC.limits = inclusive [(-1.5, 4.0), (-3.0, 4.0)]
        , HC.noise = [0.1, 0.1]
        , HC.tweakProb = 1.0
        , HC.tweakN = 5
        , HC.select = minimize f1
        , HC.stop = NoImprovements 10 }
  rg <- newStdGen
  print $ HC.optimize p rg

