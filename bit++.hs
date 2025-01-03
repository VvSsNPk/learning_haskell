getNum :: (Read a, Num a, Show a) => IO a
getNum = readLn

main :: IO ()
main = do
  x <- getNum
  recursiveComputation x 0

recursiveComputation :: Int -> Int -> IO ()
recursiveComputation 0 l = print l
recursiveComputation n l = do
  y <- getLine
  if '+' `elem` y
    then
      recursiveComputation (n - 1) (l + 1)
    else
      recursiveComputation (n - 1) (l - 1)
