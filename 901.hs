

import Text.Printf (printf)

sequenceD :: Double -> Int -> [Double]
sequenceD d1 n = take n $ go 0 d1
  where
    go a b = a : go b (exp (b - a))

energy :: Double -> Int -> Double
energy d1 n =
    let ds = sequenceD d1 n
    in sum $ zipWith (\dk dkm1 -> dk * exp (-dkm1)) (tail ds) ds

explodes :: Double -> Int -> Bool
explodes d1 n = any (> 1e15) (sequenceD d1 n)

findD1 :: Double
findD1 = bsearch 0.7 0.8 200
  where
    bsearch lo hi 0 = (lo + hi) / 2
    bsearch lo hi k =
        let m = (lo + hi) / 2
        in if explodes m 500
              then bsearch lo m (k-1)
              else bsearch m hi (k-1)

main :: IO ()
main = do
    let d1 = findD1
    printf "d_1*  = %.12f\n" d1
    let e  = energy d1 200
    printf "E*    = %.12f\n" e
    printf "Answer (9 dp): %.9f\n" e
