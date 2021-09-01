module Definition where

import           System.Random (Random (random, randomR, randoms), StdGen)

type Limit = (Double, Double)
type Prob = Double
type Solution = [Double]
type ObjFun = Solution -> Double
data StopCheck = ZChecks Int | Iterations Int | NoImprovements Int Double
data Stats = Stats { zChecks :: Int, zBest :: [Double] } deriving (Eq, Read, Show)

instance Num a => Num [a] where
  xs + ys = zipWith (+) xs ys
  xs - ys = zipWith (-) xs ys
  xs * ys = zipWith (*) xs ys
  abs xs = abs <$> xs
  signum xs = signum <$> xs
  negate xs = negate <$> xs
  fromInteger i = repeat (fromIntegral i)

infix 4 <=?<=
(<=?<=) :: Double -> Limit -> Bool
x <=?<= (lo, hi) = lo <= x && x <= hi

infix 4 <?<=
(<?<=) :: Double -> Limit -> Bool
x <?<= (lo, hi) = lo < x && x <= hi

infix 4 <=?<
(<=?<) :: Double -> Limit -> Bool
x <=?< (lo, hi) = lo <= x && x < hi

infix 4 <?<
(<?<) :: Double -> Limit -> Bool
x <?< (lo, hi) = lo < x && x < hi

minimize :: Double -> Double
minimize = id

maximize :: Double -> Double
maximize = negate

inLimits :: [Limit] -> Solution -> Bool
inLimits lim s = and (zipWith (<=?<=) s lim)

randMap :: StdGen -> (StdGen -> a -> (b, StdGen)) -> [a] -> ([b], StdGen)
randMap rg f []     = ([], rg)
randMap rg f (x:xs) = (x':xs', rg'')
  where
    (x', rg') = f rg x
    (xs', rg'') = randMap rg' f xs

randFor :: StdGen -> Int -> (StdGen -> (a, StdGen)) -> ([a], StdGen)
randFor rg n f = randMap rg (\ rg _ -> f rg) [1 .. n]

best :: Ord b => (a -> b) -> [a] -> a
best z xs = xs !! (snd . minimum) (zip (z <$> xs) [0..])

minOn :: Ord b => (a -> b) -> a -> a -> a
minOn key a b = if key a < key b then a else b

maxOn :: Ord b => (a -> b) -> a -> a -> a
maxOn key a b = if key a > key b then a else b

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
