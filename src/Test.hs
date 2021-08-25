module Test where

import           Data.List       (intercalate)
import           Definition      (randMap)
import qualified HillClimb       as HC
import qualified IterLocalSearch as ILS
import qualified PatternSearch   as PS
import           System.Random   (StdGen, newStdGen)
import           Text.Printf     (printf)

type Metrics = (Double, Double, Double, Double)
type MetricCols = (String, String, String, String, String)

showCases :: Int -> Int -> StdGen -> [(HC.Params, ILS.Params)] -> (String, StdGen)
showCases fp n rg ps = (intercalate "\n" tables, rg')
  where
    cols = ("Algoritmo", "Mínimo", "Máximo", "Média", "Desvio")
    labels = ["HC", "ILS"]
    (mss, rg') = runCases n rg ps
    tables = map (showTabularMetrics cols fp . zip labels) mss

runCases :: Int -> StdGen -> [(HC.Params, ILS.Params)] -> ([[Metrics]], StdGen)
runCases n rg = randMap rg (runCase n)

runCase :: Int -> StdGen -> (HC.Params, ILS.Params) -> ([Metrics], StdGen)
runCase n rg (pHC, pILS) = ([metrics zsHC, metrics zsILS], rg'')
  where
    (sHC, rg') = randMap rg (\ rg _ -> HC.optimize pHC rg) [1..n]
    (sILS, rg'') = randMap rg' (\ rg _ -> ILS.optimize pILS rg) [1..n]
    zsHC = map (HC.z pHC) sHC
    zsILS = map (ILS.z pILS) sILS

metrics :: [Double] -> Metrics
metrics zs = (xmin, xmax, avg, stddev)
  where
    (xmin, xmax, s, n') = foldl metrics' (-1/0, 1/0, 0.0, 0) zs
    n = fromIntegral n'
    avg = s / n
    stddev = sqrt $ sum (map (\ x -> (x - avg) ** 2) zs) / n

metrics' :: (Double, Double, Double, Int) -> Double -> (Double, Double, Double, Int)
metrics' (xmin, xmax, s, n) x = (min xmin x, max xmax x, s + x, n + 1)

showMetrics :: Int -> Int -> Metrics -> String
showMetrics sp fp (xmin, xmax, avg, std) = printf fmt xmin xmax avg std
  where fmt = intercalate "|" $ replicate 4 $ printf "%%%d.%df" sp fp

showLabeledMetrics :: Int -> Int -> Int -> [(String, Metrics)] -> String
showLabeledMetrics labelSp sp fp m = unlines $ map labeled m
  where labeled (l, m) = printf (printf "|%%%ds|" labelSp) l ++ showMetrics sp fp m ++ "|"

showTabularMetrics :: MetricCols -> Int -> [(String, Metrics)] -> String
showTabularMetrics (alg, mi, ma, avg, std) fp m = sep ++ header ++ sep ++ mLines ++ sep
  where
    sp = maximum $ map length [alg, mi, ma, avg, std]
    fmt = intercalate "|" (replicate 4 $ printf "%%%ds" sp) ++ "|\n"
    header = "|" ++ alg ++ "|" ++ printf fmt mi ma avg std
    mLines = showLabeledMetrics (length alg) sp fp m
    sep = "+---------+" ++ intercalate "+" (replicate 4 (replicate sp '-')) ++ "+\n"