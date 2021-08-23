module Main where

import           Definition
import qualified HillClimb     as HC
import           System.Random (Random (random), StdGen, newStdGen)

f1 :: [Double] -> Double
f1 [x, y] = sin (x + y) + (x - y) ** 2 - 1.5 * x + 2.5 * y + 1.0
f1 _      = error "wrong number of parameters"

main :: IO ()
main = do
  rg <- newStdGen
  let p = HC.Params
        { HC.z = f1
        , HC.limits = inclusive [(-1.5, 4.0), (-3.0, 4.0)]
        , HC.step = 0.1
        , HC.tweakProb = 1.0
        , HC.tweakN = 5
        , HC.select = minimize
        , HC.stop = NoImprovements 10 }
  print $ HC.optimize p rg
