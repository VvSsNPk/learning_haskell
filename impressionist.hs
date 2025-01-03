import Control.Monad

readNum :: (Read a, Num a, Show a) => IO a
readNum = readLn
main :: IO ()
main = do
  x <- readNum
  parseAndMap x

parseAndMap :: Int -> IO ()
parseAndMap 0 = pure ()
parseAndMap n = do
  m <- readNum
  lines <- replicateM m getLine
  let x = map words lines
  let y = map (map read) x
  let indexs = [0 .. length y - 1]
  let zz = map (\l -> [head l .. last l]) y
  let comb = combinations zz
  let ans = allIndexPairwise indexs comb
  let prin = map (\x -> if x == 1 then '1' else '0') ans
  putStrLn prin
  parseAndMap (n - 1)

pairs :: [a] -> [a] -> [[a]]
pairs xs ys = [[x, y] | x <- xs, y <- ys]

allIndexPairwise :: (Eq a, Num a) => [Int] -> [[a]] -> [Int]
allIndexPairwise [] _ = []
allIndexPairwise (x : xs) ys = (if any (elemAtIndex x) ys then 1 else 0) : allIndexPairwise xs ys

combinations :: (Eq a) => [[a]] -> [[a]]
combinations [] = [[]]
combinations (x : xs) = [m : ns | m <- x, ns <- combinations xs]

elemAtIndex :: (Eq a) => Int -> [a] -> Bool
elemAtIndex x xs =
  (x < length xs)
    && ( let el = xs !! x
             first = take x xs
             rest = drop (x + 1) xs
          in notElem el first && notElem el rest
       )
