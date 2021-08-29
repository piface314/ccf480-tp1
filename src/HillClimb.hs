module HillClimb (Params(..), optimize) where

import           Definition
import           System.Random (Random (random, randomR), StdGen)

data Params = Params
  { z         :: ObjFun
  , opt       :: Double -> Double
  , limits    :: [Limit]
  , noise     :: [Double]
  , tweakProb :: Prob
  , tweakN    :: Int
  , stop      :: StopCheck }

optimize :: Params -> StdGen -> (Solution, StdGen)
optimize p rg = optimize' p rg' (Stats 0 []) i
  where (i, rg') = initialize p rg

optimize' :: Params -> StdGen -> Stats -> Solution -> (Solution, StdGen)
optimize' p rg stats@(Stats zn zv) s =
  if shouldStop (stop p) stats
    then (s, rg)
    else optimize' p rg' stats' s'
  where
    n = tweakN p
    (candidates, rg') = randMap rg (\ rg _ -> tweak p rg s) [1 .. n]
    s' = select p (s:candidates)
    stats' = Stats (zn + n + 1) (z p s' : zv)

initialize :: Params -> StdGen -> (Solution, StdGen)
initialize p rg = randMap rg vi (limits p)
  where
    vi :: StdGen -> Limit -> (Double, StdGen)
    vi rg (lo, _, hi) = randomR (lo, hi) rg

tweak :: Params -> StdGen -> Solution -> (Solution, StdGen)
tweak p rg s = randMap rg (addNoise p) (zip3 s (noise p) (limits p))

addNoise :: Params -> StdGen -> (Double, Double, Limit) -> (Double, StdGen)
addNoise p rg i@(x, _, _) =
  let (r, rg') = random rg
    in if r <= tweakProb p
      then addNoise' rg' i
      else (x, rg')

addNoise' :: StdGen -> (Double, Double, Limit) -> (Double, StdGen)
addNoise' rg i@(x, noise, (lo, (<?), hi)) =
  let (x', rg') = randomR (x - noise, x + noise) rg
    in if x' <? (lo, hi)
      then (x', rg')
      else addNoise' rg' i

select :: Params -> [Solution] -> Solution
select p = best (opt p . z p)
