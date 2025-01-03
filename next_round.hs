main :: IO ()
main = do
  x <- getLine
  let m = map read (words x) :: [Int]
  let g = last m
  y <- getLine
  let n = map read (words y) :: [Int]
  let fil = n !! g
  let ans = filter (>= fil) (filter (> 0) n)
  print (length ans)
  pure ()
