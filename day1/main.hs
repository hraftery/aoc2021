import Day1

main = do
  contents <- getContents
  putStr $ go (lines contents)
