module Definition where

import           System.Random (Random (random, randomR, randoms), StdGen)

type Limit = (Double, Double -> (Double, Double) -> Bool, Double)
type Prob = Double
type Solution = [Double]
type ObjFun = Solution -> Double
type Selection = Solution -> Solution -> Solution
data StopCheck = ZChecks Int | Iterations Int | NoImprovements Int | NoGoodImprovements Int Double
data Stats = Stats { zChecks :: Int, improvements :: [Double] } deriving (Eq, Read, Show)

infix 4 <=?<=
(<=?<=) :: Double -> (Double, Double) -> Bool
(<=?<=) x (lo, hi) = lo <= x && x <= hi

infix 4 <?<=
(<?<=) :: Double -> (Double, Double) -> Bool
(<?<=) x (lo, hi) = lo < x && x <= hi

infix 4 <=?<
(<=?<) :: Double -> (Double, Double) -> Bool
(<=?<) x (lo, hi) = lo <= x && x < hi

infix 4 <?<
(<?<) :: Double -> (Double, Double) -> Bool
(<?<) x (lo, hi) = lo < x && x < hi

inclusive :: [(Double, Double)] -> [Limit]
inclusive = map (\(lo, hi) -> (lo, (<=?<=), hi))

exclusive :: [(Double, Double)] -> [Limit]
exclusive = map (\(lo, hi) -> (lo, (<?<), hi))

minimize :: ObjFun -> Solution -> Solution -> Solution
minimize z s1 s2 = if z s1 < z s2 then s1 else s2

maximize :: ObjFun -> Solution -> Solution -> Solution
maximize z s1 s2 = if z s1 > z s2 then s1 else s2

inLimits :: [Limit] -> Solution -> Bool
inLimits lim s = all (\((lo, chk, hi), x) -> chk x (lo, hi)) (zip lim s)

randMap :: StdGen -> (StdGen -> a -> (b, StdGen)) -> [a] -> ([b], StdGen)
randMap rg f []     = ([], rg)
randMap rg f (x:xs) = (x':xs', rg'')
  where
    (x', rg') = f rg x
    (xs', rg'') = randMap rg' f xs

shouldStop :: StopCheck -> Stats -> Bool
shouldStop (ZChecks maxn) Stats{zChecks = n}                   = n > maxn
shouldStop (Iterations maxn) Stats{improvements = i}           = length i > maxn
shouldStop (NoImprovements maxn) Stats{improvements = i}       = n > maxn
  where n = length (takeWhile (< 1e-6) i)
shouldStop (NoGoodImprovements maxn p) Stats{improvements = i} = n > maxn
  where n = length (takeWhile (< p) i)
