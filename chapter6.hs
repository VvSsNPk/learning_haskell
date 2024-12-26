sundown :: Int -> Int
sundown 0 = 0
sundown x = x + sundown (x - 1)

ex :: Int -> Int -> Int
ex x 0 = 1
ex x n = x * ex x (n - 1)

-- merge :: (Ord a) => [a] -> [a] -> [a]
-- merge [] [] = []
-- merge (x : xs) (y : ys)
--   | x <= y = [x, y] ++ merge xs ys
--   | otherwise = [y, x] ++ merge xs ys
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [x] [y]
  | x <= y = [x, y]
  | otherwise = [y, x]
merge [x] (y : ys)
  | x <= y = x : (y : ys)
  | otherwise = y : merge [x] ys
merge (x : xs) [y] = merge [y] (x : xs)
merge (x : xs) (y : ys)
  | x <= y = x : merge (merge [y] xs) ys
  | otherwise = y : merge (merge [x] ys) xs

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [a] = [a]
msort x = merge first second
 where
  (m, n) = halve x
  first = msort m
  second = msort n

halve :: [a] -> ([a], [a])
halve [] = ([], [])
halve [x, y] = ([x], [y])
halve xy = (first, second)
 where
  first = take n xy
  second = drop n xy
  n = length xy `div` 2
