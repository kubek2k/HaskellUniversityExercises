main = do
  contents <- getContents
  putStr $ (unlines . filter (\l -> length l < 10) . lines) contents
