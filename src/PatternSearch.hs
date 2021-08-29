module PatternSearch (Params(..), search) where

-- Hooke-Jeeves method / pattern search / direct search

import           Definition

data Params = Params
  { z         :: ObjFun
  , opt       :: Double -> Double
  , step      :: [Double]
  , limits    :: [Limit]
  , precision :: [Double] }

search :: Params -> Solution -> (Solution, Int)
search p = search' p (step p) 0

search' :: Params -> [Double] -> Int -> Solution -> (Solution, Int)
search' p step zn s
  | all (uncurry (<)) (zip step (precision p)) = (s, zn)
  | s == s'   = search' p (map (/ 2.0) step) zn' s
  | otherwise = search' p step zn' s'
  where
    candidates = filter (inLimits (limits p)) (move step s)
    s' = select p (s:candidates)
    zn' = zn + length candidates + 1

move :: [Double] -> Solution -> [Solution]
move step s = zip step [0 .. length s - 1] >>= move'
  where
    move' :: (Double, Int) -> [Solution]
    move' (step, i) = let (pre, x:xs) = splitAt i s in [pre ++ x+step:xs, pre ++ x-step:xs]

select :: Params -> [Solution] -> Solution
select p = best (opt p . z p)