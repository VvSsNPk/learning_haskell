getNum :: (Read a, Show a, Num a) => IO a
getNum = readLn
main :: IO ()
main = do
  x <- getNum
  recursiveProcess x

parseWord :: String -> Maybe String
parseWord x = if length x > 10 then Just ([head x] ++ show (length x - 2) ++ [last x]) else Nothing

printOutput :: Maybe String -> String -> IO ()
printOutput x y = case x of
  Just l -> putStrLn l
  Nothing -> putStrLn y

recursiveProcess :: Int -> IO ()
recursiveProcess 0 = pure ()
recursiveProcess x = do
  l <- getLine
  let y = parseWord l
  case y of
    Just g -> putStrLn g
    Nothing -> putStrLn l
  recursiveProcess (x - 1)
