module IterLocalSearch (Params(..), optimize) where

import           Definition
import qualified PatternSearch as PS
import           System.Random (Random (random, randomR), StdGen)

data Params = Params
  { z              :: ObjFun
  , limits         :: [Limit]
  , perturbTries   :: Int
  , noise          :: [Double]
  , localStep      :: [Double]
  , localPrecision :: [Double]
  , select         :: Selection
  , tolerance      :: Prob
  , stop           :: StopCheck }

optimize :: Params -> StdGen -> (Solution, StdGen)
optimize p rg = optimize' p rg' (Stats 0 []) i' [i']
  where
    (i, rg') = initialize p rg
    i' = search p i

optimize' :: Params -> StdGen -> Stats -> Solution -> [Solution] -> (Solution, StdGen)
optimize' p rg stats@(Stats zn zv) s memo =
  if shouldStop (stop p) stats
    then (best, rg)
    else case perturb p rg s memo of
      (Nothing, rg') -> (best, rg')
      (Just s', rg') ->
        let s'' = search p s'
            memo' = s'':memo
            best' = foldl1 (select p) memo'
            stats' = Stats (zn + 2 * length memo) (z p best' : zv)
            (r, rg'') = random rg' :: (Double, StdGen)
            sNext = if r <= tolerance p then s'' else best'
        in  optimize' p rg'' stats' sNext memo'
  where
    best = foldl1 (select p) memo

initialize :: Params -> StdGen -> (Solution, StdGen)
initialize p rg = randMap rg vi (limits p)
  where
    vi :: StdGen -> Limit -> (Double, StdGen)
    vi rg (lo, _, hi) = randomR (lo, hi) rg

search :: Params -> Solution -> Solution
search p = PS.search localP (localStep p)
  where localP = PS.Params
          { PS.z = z p
          , PS.limits = limits p
          , PS.select = select p
          , PS.precision = localPrecision p }

perturb :: Params -> StdGen -> Solution -> [Solution] -> (Maybe Solution, StdGen)
perturb = perturb' 0

perturb' :: Int -> Params -> StdGen -> Solution -> [Solution] -> (Maybe Solution, StdGen)
perturb' tries p rg s memo
  | tries > perturbTries p = (Nothing, rg)
  | isDistant p s' memo    = (Just s', rg')
  | otherwise              = perturb' (tries + 1) p rg' s memo
  where
    noise' = zipWith (\ st n -> (st, st + n)) (localStep p) (noise p)
    (s', rg') = randMap rg addNoise (zip3 s noise' (limits p))

addNoise :: StdGen -> (Double, (Double, Double), Limit) -> (Double, StdGen)
addNoise rg i@(x, (minNoise, maxNoise), (lo, (<?), hi)) =
  let (noise, rg') = randomR (minNoise, maxNoise) rg
      (r, rg'') = random rg' :: (Double, StdGen)
      x' = if r < 0.5 then x - noise else x + noise
    in if x' <? (lo, hi)
      then (x', rg'')
      else addNoise rg'' i

isDistant :: Params -> Solution -> [Solution] -> Bool
isDistant p s = all (isDistant' p s)

isDistant' :: Params -> Solution -> Solution -> Bool
isDistant' p s s' = all (\(d, x, x') -> abs (x - x') > d) (zip3 (localStep p) s s')
