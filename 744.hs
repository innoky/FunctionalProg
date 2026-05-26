-- Project Euler 744: What? Where? When?
--
-- f(n,p) = sum_{j=0}^{n-1} (n+1-j)/(2n+1) * C(n+j-1, j) * [p^n*(1-p)^j + p^j*(1-p)^n]
--
-- For n = 10^11 this needs careful numerical handling.
-- We compute term ratios incrementally to avoid overflow.

import Text.Printf (printf)

-- Direct computation for verification with small n
fDirect :: Integer -> Double -> Double
fDirect n p =
  let nD = fromIntegral n :: Double
      q  = 1 - p
      -- term for j: (n+1-j)/(2n+1) * C(n+j-1, j) * (p^n * q^j + p^j * q^n)
      -- Compute iteratively. Let
      --   a_j = C(n+j-1, j) * p^n * q^j     (expert wins with score n vs j)
      --   b_j = C(n+j-1, j) * p^j * q^n     (viewers win with score j vs n)
      -- a_0 = p^n, a_{j+1}/a_j = (n+j)/(j+1) * q
      -- b_0 = q^n, b_{j+1}/b_j = (n+j)/(j+1) * p
      go j a b acc
        | j >= n = acc
        | otherwise =
            let jD     = fromIntegral j
                factor = (nD + 1 - jD) / (2*nD + 1)
                acc'   = acc + factor * (a + b)
                a'     = a * (nD + jD) / (jD + 1) * q
                b'     = b * (nD + jD) / (jD + 1) * p
            in go (j+1) a' b' acc'
  in go 0 (p**nD) (q**nD) 0

main :: IO ()
main = do
  -- Verify with given values
  let v1 = fDirect 6 0.5
      v2 = fDirect 10 (3/7)
      v3 = fDirect 10000 0.3
  printf "f(6, 1/2)      = %.10f  (expected 0.2851562500)\n" v1
  printf "f(10, 3/7)     = %.10f  (expected 0.2330040743)\n" v2
  printf "f(10^4, 0.3)   = %.10f  (expected 0.2857499982)\n" v3
  -- The target
  let ans = fDirect (10^11) 0.4999
  printf "f(10^11,0.4999)= %.10f\n" ans
