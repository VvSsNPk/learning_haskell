import Data.Char (intToDigit)

getNum :: (Read a, Num a, Show a) => IO a
getNum = readLn

main :: IO ()
main = do
  n <- getNum
  parseNum n 0
  pure ()

parseNum :: Int -> Int -> IO ()
parseNum 0 n = do
  print n
parseNum x y = do
  n <- getLine
  let m = map read (words n) :: [Int]
  let c = filter (== 1) m
  if length c >= 2 then parseNum (x - 1) (y + 1) else parseNum (x - 1) y
