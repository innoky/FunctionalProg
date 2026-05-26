-- Project Euler 103: Special Subset Sums: Optimum
--
-- Найти 7-элементное "special sum set" A с минимальной суммой S(A),
-- где special означает:
--   (1) все непустые подмножества имеют разные суммы;
--   (2) если |B| > |C| (непустые, непересекающиеся), то S(B) > S(C).
--
-- Идея:
--   * Известна верхняя оценка через правило из условия:
--     для n=6 оптимум {11,18,19,20,22,25}, средний элемент b=20,
--     даёт кандидата для n=7: {20, 31, 38, 39, 40, 42, 45}, S=255.
--     Значит искомая S(A) <= 255.
--   * Перебираем возрастающие 7-наборы с суммой <= 255 - 1 = 254
--     (на самом деле мы перебираем все <= bestSum-1, обновляя bestSum
--     по мере нахождения).
--   * При построении делаем сильный прунинг:
--       - текущая частичная сумма + минимальное продолжение должно
--         не превышать предельной;
--       - проверяем "монотонность по размеру" (свойство 2) как можно
--         раньше: достаточно проверять, что сумма (k+1) наименьших
--         элементов превышает сумму k наибольших (т.е. сумма i-го+
--         следующий по списку >...). На практике проверяем по
--         достроенному префиксу.
--   * Когда набор из 7 элементов построен, проверяем единственность
--     сумм всех подмножеств.
--
-- Компилируется: ghc -O2 special_sum.hs

{-# LANGUAGE BangPatterns #-}

module Main where

import Data.IORef
import Data.List (sort)
import qualified Data.Set as Set

-- Проверка свойства 2 для отсортированного по возрастанию списка:
-- достаточно для всех k: сумма k+1 наименьших > сумма k наибольших.
-- Эквивалентная (более сильная и быстрая) проверка: для каждого i от 1 до n-1
-- a[i] > a[n-1] + a[n-2] + ... + a[n-i+1] - (a[1] + ... + a[i-1])?
-- Берём прямую формулировку для надёжности.
prop2 :: [Int] -> Bool
prop2 xs =
  let n = length xs
      arr = xs
      sumK k = sum (take k arr)             -- сумма k наименьших
      sumKBig k = sum (take k (reverse arr)) -- сумма k наибольших
  in all (\k -> sumK (k + 1) > sumKBig k) [1 .. n `div` 2]

-- Проверка свойства 1: все суммы 2^n - 1 непустых подмножеств различны.
prop1 :: [Int] -> Bool
prop1 xs =
  let subs = filter (not . null) (subsetsOf xs)
      sums = map sum subs
      n    = length sums
  in Set.size (Set.fromList sums) == n
  where
    subsetsOf []     = [[]]
    subsetsOf (y:ys) = let rest = subsetsOf ys in rest ++ map (y:) rest

-- Полная проверка special sum set.
isSSS :: [Int] -> Bool
isSSS xs = prop2 (sort xs) && prop1 xs

-- Преобразование множества в "set string" из условия: конкатенация
-- элементов по возрастанию.
setString :: [Int] -> String
setString xs = concatMap show (sort xs)

-- Поиск оптимума для n=7.
-- Подход: перебираем возрастающие 7-наборы с прунингом по сумме.
-- Поддерживаем "лучший на данный момент" в IORef.
-- Стартуем с верхней оценки из правила (255 для n=7).
solve :: IO (Int, [Int])
solve = do
  -- Стартовая верхняя оценка: применённое правило к оптимуму n=6.
  -- {20, 31, 38, 39, 40, 42, 45}, sum = 255. Ищем строго лучше.
  bestRef <- newIORef (255, [20,31,38,39,40,42,45])

  -- Эвристический нижний старт первого элемента: a1 не может быть слишком
  -- мал (свойство 2 заставляет элементы быть близкими). Для n=7 опыт
  -- (и публичные обсуждения PE) показывает, что a1 окажется около 20.
  -- Но мы не хотим строить решение на этом знании; начинаем с a1 = 1.
  let nMax = 7

      -- Рекурсия: chosen --- уже выбранный возрастающий список (в обратном
      -- порядке для эффективного добавления, последний добавленный --- голова),
      -- остаток нужно добавить = nMax - length chosen элементов.
      go :: [Int] -> Int -> Int -> IO ()
      go chosen !curSum !lastPicked = do
        (bestSum, _) <- readIORef bestRef
        let kChosen = length chosen
            need    = nMax - kChosen
        if need == 0
          then do
            let xs = reverse chosen
            if curSum < bestSum && isSSS xs
              then writeIORef bestRef (curSum, xs)
              else return ()
          else do
            -- Минимальная возможная сумма продолжения: next, next+1, ...,
            -- где next = lastPicked + 1 (мы строим строго возрастающий список).
            -- Это даёт нижнюю границу для финальной суммы.
            let minContinuation =
                  sum [lastPicked + 1 .. lastPicked + need]
                -- Максимально возможное значение следующего элемента,
                -- чтобы оставшиеся (need-1) элементов (каждый строго
                -- больше) могли быть подобраны и итог < bestSum:
                -- next + (next+1) + ... + (next+need-1) <= bestSum - 1 - curSum
                -- => need*next + need*(need-1)/2 <= bestSum-1-curSum
                budget = bestSum - 1 - curSum
                nextMaxForSum =
                  (budget - need*(need-1) `div` 2) `div` need
                nextMin = lastPicked + 1
                nextMax = nextMaxForSum
            if curSum + minContinuation >= bestSum
              then return ()
              else do
                -- Перебираем следующий элемент.
                let tryNext v
                      | v > nextMax = return ()
                      | otherwise = do
                          -- Прунинг по свойству 2 на лету:
                          -- Берём текущий список, добавляем v, сортируем
                          -- и проверяем prop2 для текущего префикса.
                          -- Однако prop2 определена только когда длина >= 2.
                          let newChosen = v : chosen
                              sortedSoFar = reverse newChosen  -- возрастающий
                              -- Проверяем "монотонность сумм" частично:
                              -- сумма 2 наименьших > наибольший?
                              -- сумма 3 наименьших > сумма 2 наибольших? и т.д.
                              -- Полная проверка: для всех k, где сравнение
                              -- имеет смысл (т.е. 2k+1 <= |sortedSoFar|).
                              k = length sortedSoFar
                              checkAll =
                                all (\j -> sum (take (j+1) sortedSoFar)
                                         > sum (take j (reverse sortedSoFar)))
                                    [1 .. (k - 1) `div` 2]
                          if checkAll
                            then go newChosen (curSum + v) v
                            else return ()
                          tryNext (v + 1)
                tryNext nextMin

  -- Стартуем; первый элемент >= 1.
  go [] 0 0
  readIORef bestRef

main :: IO ()
main = do
  putStrLn "Searching optimum special sum set for n = 7 ..."
  (s, xs) <- solve
  putStrLn $ "Optimum sum S(A) = " ++ show s
  putStrLn $ "A = " ++ show xs
  putStrLn $ "Set string: " ++ setString xs
