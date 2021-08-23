module HillClimb where

import           Definition
import           System.Random

data Params = Params
  { z         :: ObjFun
  , limits    :: [Limit]
  , step      :: Double
  , tweakProb :: Prob
  , tweakN    :: Int
  , select    :: Selection
  , stop      :: StopCheck }

optimize :: Params -> StdGen -> (Solution, Stats)
optimize p rg = (s, stats)
  where
    (i, rg') = initialize p rg
    (s, _, stats) = optimize' p rg' (Stats 0 []) i

optimize' :: Params -> StdGen -> Stats -> Solution -> (Solution, StdGen, Stats)
optimize' p rg stats@(Stats zn imp) s = if shouldStop (stop p) stats then (s, rg, stats) else optimize' p rg' stats' s'
  where
    n = tweakN p
    (candidates, rg') = randMap rg (\rg _ -> tweak p rg s) [1 .. n]
    s' = foldl (select p (z p)) s candidates
    stats' = let zv  = z p s
                 zv' = z p s' in Stats (zn + 2 * n) (abs ((zv' - zv) / zv) : imp)

initialize :: Params -> StdGen -> (Solution, StdGen)
initialize p rg = randMap rg vi (limits p)
  where
    vi :: StdGen -> Limit -> (Double, StdGen)
    vi rg (lo, _, hi) = randomR (lo, hi) rg

tweak :: Params -> StdGen -> Solution -> (Solution, StdGen)
tweak p rg s = randMap rg (addNoise p) (zip s (limits p))

addNoise :: Params -> StdGen -> (Double, Limit) -> (Double, StdGen)
addNoise prm rg i@(x, _) = if p <= tweakProb prm then addNoise' prm rg' i else (x, rg')
  where (p, rg') = random rg

addNoise' :: Params -> StdGen -> (Double, Limit) -> (Double, StdGen)
addNoise' p rg i@(x, (lo, chk, hi)) = if chk x (lo, hi) then x' else addNoise' p rg' i
  where
    st = step p
    x'@(_, rg') = randomR (x - st, x + st) rg