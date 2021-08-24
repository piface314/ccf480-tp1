module HillClimb (Params(..), optimize) where

import           Definition
import           System.Random

data Params = Params
  { z         :: ObjFun
  , limits    :: [Limit]
  , noise     :: Double
  , tweakProb :: Prob
  , tweakN    :: Int
  , select    :: Selection
  , stop      :: StopCheck }

optimize :: Params -> StdGen -> Solution
optimize p rg = s
  where
    (i, rg') = initialize p rg
    (s, _) = optimize' p rg' (Stats 0 []) i

optimize' :: Params -> StdGen -> Stats -> Solution -> (Solution, StdGen)
optimize' p rg stats@(Stats zn imp) s =
  if shouldStop (stop p) stats
    then (s, rg)
    else optimize' p rg' stats' s'
  where
    n = tweakN p
    (candidates, rg') = randMap rg (\rg _ -> tweak p rg s) [1 .. n]
    s' = foldl (select p) s candidates
    zv = z p s
    zv' = z p s'
    stats' = Stats (zn + 2 * n) (abs ((zv' - zv) / zv) : imp)

initialize :: Params -> StdGen -> (Solution, StdGen)
initialize p rg = randMap rg vi (limits p)
  where
    vi :: StdGen -> Limit -> (Double, StdGen)
    vi rg (lo, _, hi) = randomR (lo, hi) rg

tweak :: Params -> StdGen -> Solution -> (Solution, StdGen)
tweak p rg s = randMap rg (addNoise p) (zip s (limits p))

addNoise :: Params -> StdGen -> (Double, Limit) -> (Double, StdGen)
addNoise prm rg i@(x, _) =
  let (p, rg') = random rg
    in if p <= tweakProb prm
      then addNoise' prm rg' i
      else (x, rg')

addNoise' :: Params -> StdGen -> (Double, Limit) -> (Double, StdGen)
addNoise' p rg i@(x, (lo, (<?), hi)) =
  let (x', rg') = randomR (x - noise p, x + noise p) rg
    in if x' <? (lo, hi)
      then (x', rg')
      else addNoise' p rg' i
