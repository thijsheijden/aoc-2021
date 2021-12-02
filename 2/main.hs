import Data.List

main :: IO()
main = do
  input <- readFile "input.txt"
  
  let x = f (map words $ lines input) 0 0

  print x

f :: [[String]] -> Int -> Int -> Int
f (x:xs) h v  | "forward" `isPrefixOf` head x = f xs (h + read (last x)  :: Int) v
              | "backward" `isPrefixOf` head x = f xs (h + read (last x) :: Int) v
              | "up" `isPrefixOf` head x = f xs h (v - read (last x) :: Int)
              | "down" `isPrefixOf` head x = f xs h (v + read (last x) :: Int)
f [] h v = h * v