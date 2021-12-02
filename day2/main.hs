import Day2

main = do
  contents <- getContents
  putStr $ go (lines contents)
