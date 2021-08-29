module Definition where

import           System.Random (Random (random, randomR, randoms), StdGen)

type Limit = (Double, Double -> (Double, Double) -> Bool, Double)
type Prob = Double
type Solution = [Double]
type ObjFun = Solution -> Double
type Selection = Solution -> Solution -> Solution
data StopCheck = ZChecks Int | Iterations Int | NoImprovements Int Double
data Stats = Stats { zChecks :: Int, zBest :: [Double] } deriving (Eq, Read, Show)

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
inclusive = map (\ (lo, hi) -> (lo, (<=?<=), hi))

exclusive :: [(Double, Double)] -> [Limit]
exclusive = map (\ (lo, hi) -> (lo, (<?<), hi))

minimize :: Double -> Double
minimize = id

maximize :: Double -> Double
maximize = negate

inLimits :: [Limit] -> Solution -> Bool
inLimits lim s = all (\ ((lo, (<?), hi), x) -> x <? (lo, hi)) (zip lim s)

randMap :: StdGen -> (StdGen -> a -> (b, StdGen)) -> [a] -> ([b], StdGen)
randMap rg f []     = ([], rg)
randMap rg f (x:xs) = (x':xs', rg'')
  where
    (x', rg') = f rg x
    (xs', rg'') = randMap rg' f xs

randFor :: StdGen -> (StdGen -> (a, StdGen)) -> Int -> ([a], StdGen)
randFor rg f n = randMap rg (\ rg _ -> f rg) [1 .. n]

best :: Ord b => (a -> b) -> [a] -> a
best z xs = xs !! (snd . minimum) (zip (map z xs) [0..])

shouldStop :: StopCheck -> Stats -> Bool
shouldStop (ZChecks maxn) Stats{zChecks = n}        = n > maxn
shouldStop (Iterations maxn) Stats{zBest = v}       = length v > maxn
shouldStop (NoImprovements maxn p) Stats{zBest = v} = n > maxn
  where
    n = length (takeWhile (< p) (improvement v))
    improvement :: [Double] -> [Double]
    improvement []            = []
    improvement [_]           = []
    improvement (x':xs@(x:_)) = abs ((x' - x) / x) : improvement xs
