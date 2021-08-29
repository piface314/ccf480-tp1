module IterLocalSearch (Params(..), optimize) where

import           Definition
import           System.Random (Random (random, randomR), StdGen)

data Params = Params
  { z              :: ObjFun
  , opt            :: Double -> Double
  , limits         :: [Limit]
  , perturbTries   :: Int
  , noise          :: [(Double, Double)]
  , search         :: Solution -> (Solution, Int)
  , tolerance      :: Prob
  , stop           :: StopCheck }

optimize :: Params -> StdGen -> (Solution, StdGen)
optimize p rg = optimize' p rg' (Stats zn [z p i']) i' [i']
  where
    (i, rg') = initialize p rg
    (i', zn) = search p i

optimize' :: Params -> StdGen -> Stats -> Solution -> [Solution] -> (Solution, StdGen)
optimize' p rg stats@(Stats zn zv) s memo =
  if shouldStop (stop p) stats
    then (sBest, rg)
    else case perturb p rg s memo of
      (Nothing, rg') -> (sBest, rg')
      (Just s', rg') ->
        let (s'', zn') = search p s'
            memo' = s'':memo
            sBest' = select p memo'
            stats' = Stats (zn + zn' + length memo') (z p sBest' : zv)
            (r, rg'') = random rg'
            sNext = if r <= tolerance p then s'' else sBest'
        in  optimize' p rg'' stats' sNext memo'
  where
    sBest = select p memo

initialize :: Params -> StdGen -> (Solution, StdGen)
initialize p rg = randMap rg vi (limits p)
  where
    vi :: StdGen -> Limit -> (Double, StdGen)
    vi rg (lo, _, hi) = randomR (lo, hi) rg

perturb :: Params -> StdGen -> Solution -> [Solution] -> (Maybe Solution, StdGen)
perturb = perturb' 0

perturb' :: Int -> Params -> StdGen -> Solution -> [Solution] -> (Maybe Solution, StdGen)
perturb' tries p rg s memo
  | tries > perturbTries p = (Nothing, rg)
  | isDistant p s' memo    = (Just s', rg')
  | otherwise              = perturb' (tries + 1) p rg' s memo
  where
    (s', rg') = randMap rg addNoise (zip3 s (noise p) (limits p))

addNoise :: StdGen -> (Double, (Double, Double), Limit) -> (Double, StdGen)
addNoise rg i@(x, (minNoise, maxNoise), (lo, (<?), hi)) =
  let (noise, rg') = randomR (minNoise, maxNoise) rg
      (r, rg'') = random rg' :: (Prob, StdGen)
      x' = if r < 0.5 then x - noise else x + noise
    in if x' <? (lo, hi)
      then (x', rg'')
      else addNoise rg'' i

isDistant :: Params -> Solution -> [Solution] -> Bool
isDistant p s = all (isDistant' p s)

isDistant' :: Params -> Solution -> Solution -> Bool
isDistant' p s s' = all (\(d, x, x') -> abs (x - x') > d) (zip3 (map fst (noise p)) s s')

select :: Params -> [Solution] -> Solution
select p = best (opt p . z p)
