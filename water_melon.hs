getNum :: (Read a, Num a, Show a) => IO a
getNum = readLn

main :: IO ()
main = do
  x <- getNum
  if program x
    then
      putStrLn "Yes"
    else putStrLn "No"

all_combinations :: Int -> [(Int, Int)]
all_combinations y = [(m, n) | m <- [1 .. y], n <- [1 .. y], m + n == y]

is_even_tuple :: (Int, Int) -> Bool
is_even_tuple (m, n) = even m && even n

checl_is_even :: [(Int, Int)] -> Bool
checl_is_even = any is_even_tuple

program :: Int -> Bool
program x = checl_is_even (all_combinations x)
