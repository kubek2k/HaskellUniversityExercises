import System.IO
  {-
   -h <- openFile "cat.hs" ReadMode
   -contents <- hGetContents h
   -putStr contents
   -hClose h
   -}

main :: IO ()
main =
  withFile
    "cat.hs"
    ReadMode
    (\h -> do
       contents <- hGetContents h
       putStr contents)
