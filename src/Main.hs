module Main where

import           Definition
import qualified HillClimb          as HC
import qualified IterLocalSearch    as ILS
import qualified PatternSearch      as PS
import           System.Environment (getArgs)
import           System.Random      (newStdGen)
import qualified Test

main :: IO ()
main = do
  putStrLn "Executando algoritmos..."
  rg <- newStdGen
  args <- getArgs
  let (fp, n) = case args of
                  []     -> ("out.csv", 30)
                  [n]    -> ("out.csv", read n)
                  fp:n:_ -> (fp, read n)
  let tcases = [case1aHC, case1aILS, case1bHC, case1bILS, case2cHC, case2cILS, case2dHC, case2dILS]
  let (out, _) = Test.runCases n rg tcases
  let csv = Test.showCases ["case", "algorithm", "x", "y", "z"] out
  writeFile fp csv
  putStrLn "OK!"

f1 :: ObjFun
f1 [x, y] = sin (x + y) + (x - y) ** 2 - 1.5 * x + 2.5 * y + 1.0
f1 _      = error "wrong number of parameters"

f2 :: ObjFun
f2 [x, y] = -y47 * sinSqrtAbs (x / 2 + y47) - x * sinSqrtAbs (x - y47)
  where
    sinSqrtAbs = sin . sqrt . abs
    y47 = y + 47
f2 _      = error "wrong number of parameters"

limits1a = inclusive [(-1.5, 4.0), (-3.0, 4.0)]
limits1b = inclusive [(-1.0, 0.0), (-2.0, -1.0)]
limits2c = inclusive [(-512.0, 512.0), (-512.0, 512.0)]
limits2d = inclusive [(511.0, 512.0), (404.0, 405.0)]

case1aHC = Test.HCTestCase "1a,HC" HC.Params
              { HC.z         = f1
              , HC.opt       = minimize
              , HC.limits    = limits1a
              , HC.noise     = [0.1, 0.1]
              , HC.tweakProb = 1.0
              , HC.tweakN    = 5
              , HC.stop      = NoImprovements 10 1e-6 }

case1aILS = Test.ILSTestCase "1a,ILS" ILS.Params
              { ILS.z              = f1
              , ILS.opt            = minimize
              , ILS.limits         = limits1a
              , ILS.perturbTries   = 5
              , ILS.noise          = [(1.0, 2.0), (1.0, 2.0)]
              , ILS.tolerance      = 0.5
              , ILS.stop           = NoImprovements 10 1e-6
              , ILS.search         = PS.search PS.Params
                                      { PS.z = f1
                                      , PS.opt = minimize
                                      , PS.limits = limits1a
                                      , PS.step = [1.0, 1.0]
                                      , PS.precision = [1e-4, 1e-4] } }

case1bHC = Test.HCTestCase "1b,HC" HC.Params
              { HC.z         = f1
              , HC.opt       = minimize
              , HC.limits    = limits1b
              , HC.noise     = [0.1, 0.1]
              , HC.tweakProb = 1.0
              , HC.tweakN    = 5
              , HC.stop      = NoImprovements 10 1e-6 }

case1bILS = Test.ILSTestCase "1b,ILS" ILS.Params
              { ILS.z              = f1
              , ILS.opt            = minimize
              , ILS.limits         = limits1b
              , ILS.perturbTries   = 5
              , ILS.noise          = [(0.1, 0.5), (0.1, 0.5)]
              , ILS.tolerance      = 0.5
              , ILS.stop           = NoImprovements 10 1e-6
              , ILS.search         = PS.search PS.Params
                                      { PS.z = f1
                                      , PS.opt = minimize
                                      , PS.limits = limits1b
                                      , PS.step = [0.1, 0.1]
                                      , PS.precision = [1e-4, 1e-4] } }

case2cHC = Test.HCTestCase "2c,HC" HC.Params
              { HC.z         = f2
              , HC.opt       = minimize
              , HC.limits    = limits2c
              , HC.noise     = [10.0, 10.0]
              , HC.tweakProb = 1.0
              , HC.tweakN    = 10
              , HC.stop      = NoImprovements 10 1e-6 }

case2cILS = Test.ILSTestCase "2c,ILS" ILS.Params
              { ILS.z              = f2
              , ILS.opt            = minimize
              , ILS.limits         = limits2c
              , ILS.perturbTries   = 10
              , ILS.noise          = [(10.0, 200.0), (10.0, 200.0)]
              , ILS.tolerance      = 0.5
              , ILS.stop           = NoImprovements 10 1e-6
              , ILS.search         = PS.search PS.Params
                                      { PS.z = f2
                                      , PS.opt = minimize
                                      , PS.limits = limits2c
                                      , PS.step = [10.0, 10.0]
                                      , PS.precision = [1e-2, 1e-2] } }

case2dHC = Test.HCTestCase "2d,HC" HC.Params
              { HC.z         = f2
              , HC.opt       = minimize
              , HC.limits    = limits2d
              , HC.noise     = [0.2, 0.2]
              , HC.tweakProb = 1.0
              , HC.tweakN    = 5
              , HC.stop      = NoImprovements 10 1e-6 }

case2dILS = Test.ILSTestCase "2d,ILS" ILS.Params
              { ILS.z              = f2
              , ILS.opt            = minimize
              , ILS.limits         = limits2d
              , ILS.perturbTries   = 5
              , ILS.noise          = [(0.1, 0.5), (0.1, 0.5)]
              , ILS.tolerance      = 0.5
              , ILS.stop           = NoImprovements 10 1e-6
              , ILS.search         = PS.search PS.Params
                                      { PS.z = f2
                                      , PS.opt = minimize
                                      , PS.limits = limits2d
                                      , PS.step = [0.1, 0.1]
                                      , PS.precision = [1e-4, 1e-4] } }
