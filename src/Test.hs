module Test (TestCase(..), showCases, runCases) where

import           Data.List       (intercalate)
import           Definition      (ObjFun, Solution, randFor, randMap)
import qualified HillClimb       as HC
import qualified IterLocalSearch as ILS
import qualified PatternSearch   as PS
import           System.Random   (StdGen, newStdGen)

data TestCase = TestCase String ObjFun (StdGen -> (Solution, StdGen))
type TestOut = (String, Solution, Double)

showCases :: [String] -> [TestOut] -> String
showCases header outs = unlines $ intercalate "," header : (showTestOut <$> outs)

showTestOut :: TestOut -> String
showTestOut (label, s, zv) = intercalate "," [label, intercalate "," (show <$> s), show zv]

runCases :: Int -> StdGen -> [TestCase] -> ([TestOut], StdGen)
runCases n rg tcases = (concat outs, rg')
  where (outs, rg') = randMap rg (runCase n) tcases

runCase :: Int -> StdGen -> TestCase -> ([TestOut], StdGen)
runCase n rg (TestCase label z optimize) = (testOut label z ss, rg)
  where (ss, rg') = randFor rg n optimize

testOut :: String -> ObjFun -> [Solution] -> [TestOut]
testOut label z ss = zip3 (repeat label) ss (z <$> ss)
