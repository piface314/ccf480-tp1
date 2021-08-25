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
  rg <- newStdGen
  args <- getArgs
  let (fp, n) = case args of
                  fp:n:_ -> (read fp, read n)
                  _      -> (5, 30)
  let (tables, _) = Test.showCases fp n rg [params1a, params1b, params2c, params2d]
  putStr tables

f1 :: ObjFun
f1 [x, y] = sin (x + y) + (x - y) ** 2 - 1.5 * x + 2.5 * y + 1.0
f1 _      = error "wrong number of parameters"

f2 :: ObjFun
f2 [x, y] = -y47 * sinSqrtAbs (x / 2 + y47) - x * sinSqrtAbs (x - y47)
  where
    sinSqrtAbs = sin . sqrt . abs
    y47 = y + 47
f2 _      = error "wrong number of parameters"

limits1a :: [Limit]
limits1a = inclusive [(-1.5, 4.0), (-3.0, 4.0)]
limits1b :: [Limit]
limits1b = inclusive [(-1.0, 0.0), (-2.0, -1.0)]
limits2c :: [Limit]
limits2c = inclusive [(-512.0, 512.0), (-512.0, 512.0)]
limits2d :: [Limit]
limits2d = inclusive [(511.0, 512.0), (404, 405)]

params1a :: (HC.Params, ILS.Params)
params1a = (HC.Params
              { HC.z = f1
              , HC.limits = limits1a
              , HC.noise = [0.1, 0.1]
              , HC.tweakProb = 1.0
              , HC.tweakN = 5
              , HC.select = minimize f1
              , HC.stop = NoImprovements 10 1e-6 },
            ILS.Params
              { ILS.z              = f1
              , ILS.limits         = limits1a
              , ILS.perturbTries   = 5
              , ILS.noise          = [1.0, 1.0]
              , ILS.localStep      = [1.0, 1.0]
              , ILS.localPrecision = [1e-4, 1e-4]
              , ILS.select         = minimize f1
              , ILS.tolerance      = 0.5
              , ILS.stop           = NoImprovements 10 1e-6 }
            )

params1b :: (HC.Params, ILS.Params)
params1b = (HC.Params
              { HC.z = f1
              , HC.limits = limits1b
              , HC.noise = [0.1, 0.1]
              , HC.tweakProb = 1.0
              , HC.tweakN = 5
              , HC.select = minimize f1
              , HC.stop = NoImprovements 10 1e-6 },
            ILS.Params
              { ILS.z              = f1
              , ILS.limits         = limits1b
              , ILS.perturbTries   = 5
              , ILS.noise          = [0.4, 0.4]
              , ILS.localStep      = [0.1, 0.1]
              , ILS.localPrecision = [1e-4, 1e-4]
              , ILS.select         = minimize f1
              , ILS.tolerance      = 0.5
              , ILS.stop           = NoImprovements 10 1e-6 }
            )
params2c = (HC.Params
              { HC.z = f2
              , HC.limits = limits2c
              , HC.noise = [10.0, 10.0]
              , HC.tweakProb = 1.0
              , HC.tweakN = 10
              , HC.select = minimize f2
              , HC.stop = NoImprovements 10 1e-6 },
            ILS.Params
              { ILS.z              = f2
              , ILS.limits         = limits2c
              , ILS.perturbTries   = 10
              , ILS.noise          = [100.0, 100.0]
              , ILS.localStep      = [10.0, 10.0]
              , ILS.localPrecision = [1e-2, 1e-2]
              , ILS.select         = minimize f2
              , ILS.tolerance      = 0.5
              , ILS.stop           = NoImprovements 10 1e-6 }
            )
params2d = (HC.Params
              { HC.z = f2
              , HC.limits = limits2d
              , HC.noise = [0.2, 0.2]
              , HC.tweakProb = 1.0
              , HC.tweakN = 5
              , HC.select = minimize f2
              , HC.stop = NoImprovements 10 1e-6 },
            ILS.Params
              { ILS.z              = f2
              , ILS.limits         = limits2d
              , ILS.perturbTries   = 5
              , ILS.noise          = [0.4, 0.4]
              , ILS.localStep      = [0.1, 0.1]
              , ILS.localPrecision = [1e-4, 1e-4]
              , ILS.select         = minimize f2
              , ILS.tolerance      = 0.5
              , ILS.stop           = NoImprovements 10 1e-6 }
            )
