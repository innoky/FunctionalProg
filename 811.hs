{-
  Project Euler Problem 811 — Bitwise Recursion
  ---------------------------------------------------------------------------
  b(n)      = наибольшая степень двойки, делящая n (младший установленный бит)
  A(0)      = 1
  A(2n)     = 3*A(n) + 5*A(2n - b(n))      для n > 0
  A(2n+1)   = A(n)
  H(t,r)    = A((2^t + 1)^r)

  Ключевая структура (выведена и проверена перебором):
      A(2n+1) = A(n)
      A(2n)   = c_{p(n)} * A(n),
  где p(n) — число единичных битов в n (popcount), а
      c_k = (35 * 5^(k-1) - 3) / 4,   c_1=8, c_2=43, c_3=218, ...
      (рекуррентно c_k = 5*c_{k-1} + 3, c_1 = 8).

  Отсюда, читая двоичную запись n от старшего бита к младшему,
  каждый НУЛЕВОЙ бит домножает результат на c_K, где K — количество
  единиц, встреченных в префиксе ВЫШЕ этого нуля. Единичные биты лишь
  увеличивают счётчик единиц.

      A(n) = произведение c_{(единиц в префиксе)} по всем нулевым битам n.

  Число N = (2^t + 1)^r = sum_{k=0}^r C(r,k) * 2^{t*k}.
  При большом t (t больше битовой длины любого C(r,k)) блоки C(r,k)
  не перекрываются и разделены длинными прогонами нулей. Внутри каждого
  прогона счётчик единиц постоянен, поэтому вклад прогона длины L при
  K единицах в префиксе равен c_K^L (быстрое возведение в степень по модулю).

  Для t = 10^14 + 31, r = 62: max битовая длина C(62,k) = 59 << t — наложений нет.
-}

module Main where

import Data.Bits (popCount, shiftR, (.&.))

-- модуль ответа
modulus :: Integer
modulus = 1000062031

-- множитель при добавлении нуля к числу с k единицами в префиксе:
--   c_k = (35 * 5^(k-1) - 3) / 4
cmul :: Int -> Integer
cmul k = (35 * 5 ^ (k - 1) - 3) `div` 4

cmulMod :: Int -> Integer
cmulMod k = cmul k `mod` modulus

-- быстрое возведение в степень по модулю
powMod :: Integer -> Integer -> Integer -> Integer
powMod _ 0 _ = 1
powMod base e m
  | even e    = let h = powMod base (e `div` 2) m in (h * h) `mod` m
  | otherwise = (base `mod` m) * powMod base (e - 1) m `mod` m

-- биномиальные коэффициенты строки r: [C(r,0), C(r,1), ..., C(r,r)]
binomRow :: Int -> [Integer]
binomRow r = scanl step 1 [1 .. r]
  where step c i = c * fromIntegral (r - i + 1) `div` fromIntegral i

-- битовая длина положительного целого
bitLength :: Integer -> Int
bitLength = go 0
  where go acc 0 = acc
        go acc x = go (acc + 1) (x `shiftR` 1)

-- список битов числа от старшего к младшему (для положительного n)
bitsMSB :: Integer -> [Int]
bitsMSB n = reverse (go n)
  where go 0 = []
        go x = fromIntegral (x .&. 1) : go (x `shiftR` 1)

-- H(t,r) mod modulus с использованием блочной структуры.
-- Идём по блокам от k=r (старший) к k=0 (младший).
-- Состояние: (накопленное произведение mod, число единиц в префиксе).
hMod :: Integer -> Int -> Integer
hMod t r = fst (foldl processBlock (1, 0) [r, r-1 .. 0])
  where
    coeffs = binomRow r            -- coeffs !! k = C(r,k)
    cAt k  = coeffs !! k

    processBlock :: (Integer, Int) -> Int -> (Integer, Int)
    processBlock (res, ones) k =
      let wk            = cAt k
          -- обрабатываем собственные биты блока (старший→младший)
          (res1, ones1) = foldl stepBit (res, ones) (bitsMSB wk)
          -- прогон нулей до следующего (более младшего) блока k-1
          (res2, ones2)
            | k > 0 =
                let lNext = bitLength (cAt (k - 1))
                    -- позиции нулей строго между младшим битом блока k (t*k)
                    -- и старшим битом блока k-1 (t*(k-1)+lNext-1):
                    -- их количество = t - lNext
                    gap   = t * fromIntegral k - 1
                              - (t * fromIntegral (k - 1) + fromIntegral lNext)
                              + 1
                in if gap > 0
                     then ((res1 * powMod (cmulMod ones1) gap modulus) `mod` modulus, ones1)
                     else (res1, ones1)
            | otherwise = (res1, ones1)
      in (res2, ones2)

    stepBit :: (Integer, Int) -> Int -> (Integer, Int)
    stepBit (res, ones) 1 = (res, ones + 1)                     -- единичный бит: +1 к счётчику
    stepBit (res, ones) _ = ((res * cmulMod ones) `mod` modulus, ones)  -- нулевой бит: домножаем

main :: IO ()
main = do
  -- Проверка данного значения: H(3,2) = A(81) = 636056
  putStrLn $ "H(3,2)  = " ++ show (hMod 3 2) ++ "  (ожидается 636056)"
  -- Ответ
  let t = 10 ^ (14 :: Integer) + 31
      r = 62
  putStrLn $ "H(10^14 + 31, 62) mod 1000062031 = " ++ show (hMod t r)
