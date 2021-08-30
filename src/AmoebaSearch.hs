module AmoebaSearch where

-- Nelder–Mead method / downhill simplex method / amoeba method / polytope method

import           Data.List  (sortOn)
import           Definition (Limit, ObjFun, Solution, inLimits, maxOn, (<=?<))

data Params = Params
  { z         :: ObjFun
  , opt       :: Double -> Double
  , limits    :: [Limit]
  , precision :: Double
  , initStep  :: [Double]
  , coeff     :: Coeff }

data Coeff = Coeff
  { alpha :: Double
  , gamma :: Double
  , rho   :: Double
  , sigma :: Double }

defaultCoeff :: Coeff
defaultCoeff = Coeff 1.0 2.0 0.5 0.5

alpha' :: Params -> Double
alpha' = alpha . coeff
gamma' :: Params -> Double
gamma' = gamma . coeff
rho' :: Params -> Double
rho' = rho . coeff
sigma' :: Params -> Double
sigma' = sigma . coeff

search :: Params -> Solution -> (Solution, Int)
search p x = search' p 0 (initialize p x)

search' :: Params -> Int -> [Solution] -> (Solution, Int)
search' p zn xs
  | shouldStop p xs         = (head xs', zn + length xs)
  | zxr <=?< (zx1, z' p xn) = search' p zn' (xr : xsInit)
  | zxr < zx1               = search' p (zn' + 1) ((if z' p xe < zxr then xe else xr) : xsInit)
  | z' p xc < z' p x'       = search' p (zn' + 3) (xc : xsInit)
  | otherwise               = search' p (zn' + 3) (shrink p xs')
  where
    xs' = order p xs
    (xsInit, x':_) = splitAt (length xs' - 1) xs'
    x0 = centroid xsInit
    x1 = head xsInit
    xn = last xsInit
    xr = reflection p x0 x'
    xe = expansion p x0 xr
    xc = contraction p x0 x'
    xs'' = shrink p xs'
    zx1 = z' p x1
    zxr = z' p xr
    zn' = zn + length xs + 3

initialize :: Params -> Solution -> [Solution]
initialize p x = adjustInit p poly
  where
    dist = zipWith (\ (lo, _, hi) x -> (lo - x, hi - x)) (limits p) x
    step = initStep p * (signum . uncurry (maxOn abs) <$> dist)
    poly = x : zipWith move step [0..]
    move :: Double -> Int -> Solution
    move s i = let (pre, x':xs) = splitAt i x in pre ++ x' + s : xs

adjustInit :: Params -> [Solution] -> [Solution]
adjustInit p poly =
  if all (inLimits (limits p)) poly
    then poly
    else adjustInit p (shrink p poly)

z' :: Params -> Solution -> Double
z' p = opt p . z p

order :: Params -> [Solution] -> [Solution]
order p = sortOn (z' p)

shouldStop :: Params -> [Solution] -> Bool
shouldStop p xs = all ((< precision p) . dist xc) xs
  where
    xc = centroid xs
    dist :: Solution -> Solution -> Double
    dist a b = (sqrt . sum) ((a - b) * (a - b))

centroid :: [Solution] -> Solution
centroid xs = (/ n) <$> sum xs
  where n = fromIntegral (length xs)

reflection :: Params -> Solution -> Solution -> Solution
reflection p = reflection' p (alpha' p)

reflection' :: Params -> Double -> Solution -> Solution -> Solution
reflection' p alpha x0 x' =
  if inLimits (limits p) xr
    then xr
    else reflection' p (alpha * sigma' p) x0 x'
  where xr = x0 + repeat alpha * (x0 - x')

expansion :: Params -> Solution -> Solution -> Solution
expansion p = expansion' p (gamma' p)

expansion' :: Params -> Double -> Solution -> Solution -> Solution
expansion' p gamma x0 xr =
  if inLimits (limits p) xe
    then xe
    else expansion' p (gamma * sigma' p) x0 xr
  where xe = xr + repeat gamma * (xr - x0)

contraction :: Params -> Solution -> Solution -> Solution
contraction p x0 x' = x0 + repeat (rho' p) * (x' - x0)

shrink :: Params -> [Solution] -> [Solution]
shrink p [] = error "No polytope"
shrink p (x1:xs) = x1 : xs'
  where xs' = [x1 + repeat (sigma' p) * (xi - x1) | xi <- xs]
