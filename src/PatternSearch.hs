-- https://doi.org/10.1145/321062.321069 usar Hook-Jeeves/Pattern Search/Direct Search na busca local
module PatternSearch (Params(..), search) where

import           Definition

data Params = Params
  { z         :: ObjFun
  , limits    :: [Limit]
  , select    :: Selection
  , precision :: Double }

search :: Params -> Double -> Solution -> Solution
search p step s
  | step < precision p = s
  | s == s'            = search p (step / 2.0) s
  | otherwise          = search p step s'
  where
    candidates = filter (inLimits (limits p)) (move step s)
    s' = foldl (select p) s candidates

move :: Double -> Solution -> [Solution]
move step s = [0 .. length s - 1] >>= (delta . flip splitAt s)
  where
    delta :: (Solution, Solution) -> [Solution]
    delta (i, [])   = []
    delta (i, x:xs) = [i ++ x+step:xs, i ++ x-step:xs]
