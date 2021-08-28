module Test where

import           Data.List       (intercalate)
import           Definition      (ObjFun, Solution, randFor, randMap)
import qualified HillClimb       as HC
import qualified IterLocalSearch as ILS
import qualified PatternSearch   as PS
import           System.Random   (StdGen, newStdGen)

data TestCase = HCTestCase String HC.Params | ILSTestCase String ILS.Params
type TestOut = (String, Solution, Double)

showCases :: Int -> StdGen -> [String] -> [TestCase] -> (String, StdGen)
showCases n rg header tcases = (unlines outs', rg')
  where
    (outs, rg') = runCases n rg tcases
    outs' = intercalate "," header : map showTestOut outs

showTestOut :: TestOut -> String
showTestOut (label, s, zv) = intercalate "," [label, intercalate "," (map show s), show zv]

runCases :: Int -> StdGen -> [TestCase] -> ([TestOut], StdGen)
runCases n rg tcases = (concat outs, rg')
  where (outs, rg') = randMap rg (runCase n) tcases

runCase :: Int -> StdGen -> TestCase -> ([TestOut], StdGen)
runCase n rg (HCTestCase label p) = (testOut label (HC.z p) ss, rg)
  where (ss, rg') = randFor rg (HC.optimize p) n
runCase n rg (ILSTestCase label p) = (testOut label (ILS.z p) ss, rg)
  where (ss, rg') = randFor rg (ILS.optimize p) n

testOut :: String -> ObjFun -> [Solution] -> [TestOut]
testOut label z ss = zip3 (repeat label) ss (map z ss)
