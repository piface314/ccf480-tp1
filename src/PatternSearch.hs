-- https://doi.org/10.1145/321062.321069 usar Hook-Jeeves/Pattern Search/Direct Search na busca local
module PatternSearch (Params(..), search) where

import           Definition

data Params = Params
  { z         :: ObjFun
  , limits    :: [Limit]
  , select    :: Selection
  , precision :: [Double] }

search :: Params -> [Double] -> Solution -> Solution
search p step s
  | all (uncurry (<)) (zip step (precision p)) = s
  | s == s'   = search p (map (/ 2.0) step) s
  | otherwise = search p step s'
  where
    candidates = filter (inLimits (limits p)) (move step s)
    s' = foldl (select p) s candidates

move :: [Double] -> Solution -> [Solution]
move step s = zip step [0 .. length s - 1] >>= move'
  where
    move' :: (Double, Int) -> [Solution]
    move' (step, i) = let (pre, x:xs) = splitAt i s in [pre ++ x+step:xs, pre ++ x-step:xs]
