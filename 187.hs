-- Project Euler 187: Semiprimes
--
-- Сколько составных n < 10^8 имеют ровно два (не обязательно различных)
-- простых делителя, то есть n = p*q с простыми p <= q?
--
-- Алгоритм:
--   1) Решетом Эратосфена находим все простые до N = 10^8.
--   2) Строим массив prefixPi: prefixPi[x] = количество простых <= x
--      (через префиксные суммы по решету).
--   3) Перебираем простые p с p*p < N. Для каждого считаем число
--      простых q с p <= q <= (N-1) `div` p; суммируем.
--
-- Сложность: O(N log log N) на решето, O(sqrt N) на финальный цикл.
-- Память: ~100 МБ для битмаски + ~400 МБ для prefixPi (Int).
-- Чтобы уложиться в память, prefixPi храним как UArray Int32.
--
-- Компилируется: ghc -O2 semiprimes.hs

{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Monad (when, forM_)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STUArray, newArray, readArray, writeArray, freeze)
import Data.Array.Unboxed (UArray, (!))
import Data.Int (Int32)

limit :: Int
limit = 100000000  -- 10^8 (исключительно: n < limit)

-- Решето Эратосфена: возвращает UArray Int Bool,
-- где a!i = True означает "i --- простое". Индексы 0..limit-1.
sieve :: Int -> UArray Int Bool
sieve n = runST $ do
  arr <- newArray (0, n - 1) True :: ST s (STUArray s Int Bool)
  writeArray arr 0 False
  writeArray arr 1 False
  let sqrtN = floor (sqrt (fromIntegral n :: Double)) :: Int
  forM_ [2..sqrtN] $ \i -> do
    isP <- readArray arr i
    when isP $
      forM_ [i*i, i*i + i .. n - 1] $ \j ->
        writeArray arr j False
  freeze arr

-- prefixPi[x] = количество простых p, удовлетворяющих p <= x,
-- определено для x в [0, limit-1].
-- Используем Int32, потому что pi(10^8) ~ 5.76 * 10^6, влезает.
buildPrefixPi :: UArray Int Bool -> UArray Int Int32
buildPrefixPi primes = runST $ do
  let n = limit
  arr <- newArray (0, n - 1) 0 :: ST s (STUArray s Int Int32)
  let go !i !acc
        | i >= n    = return ()
        | otherwise = do
            let !acc' = if primes ! i then acc + 1 else acc
            writeArray arr i acc'
            go (i + 1) acc'
  go 0 0
  freeze arr

main :: IO ()
main = do
  putStrLn "Строим решето Эратосфена до 10^8 ..."
  let primes = sieve limit
  putStrLn "Строим префиксные суммы pi(x) ..."
  let prefixPi = buildPrefixPi primes

  -- Проверка: pi(30) должно быть 10 (простые 2,3,5,7,11,13,17,19,23,29).
  putStrLn $ "Проверка: pi(30) = " ++ show (prefixPi ! 30) ++ " (ожидается 10)"

  -- Маленькая проверка: число полупростых ниже 30 должно быть 10.
  let countBelow upper =
        let bound = upper - 1  -- считаем n <= bound
            sqB   = floor (sqrt (fromIntegral bound :: Double)) :: Int
            piAt x | x < 0          = 0
                   | x >= limit     = error "out of range"
                   | otherwise      = fromIntegral (prefixPi ! x) :: Integer
            step !acc !p
              | p > sqB            = acc
              | not (primes ! p)   = step acc (p + 1)
              | otherwise =
                  let qMax  = bound `div` p             -- p*q <= bound, т.е. q <= bound/p
                      contribution = piAt qMax - piAt (p - 1)
                  in step (acc + contribution) (p + 1)
        in step 0 2

  putStrLn $ "Проверка: количество полупростых < 30 = "
          ++ show (countBelow 30) ++ " (ожидается 10)"

  let answer = countBelow limit
  putStrLn $ "Ответ: количество полупростых < 10^8 = " ++ show answer
