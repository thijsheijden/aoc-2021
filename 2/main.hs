import Data.List

main :: IO()
main = do
  input <- readFile "input.txt"
  
  let x = f (map words $ lines input) 0 0

  let x2 = f2 (map words $ lines input) 0 0 0

  print x
  print x2

f :: [[String]] -> Int -> Int -> Int
f (x:xs) h v  | "forward" `isPrefixOf` head x = f xs (h + read (last x)  :: Int) v
              | "backward" `isPrefixOf` head x = f xs (h + read (last x) :: Int) v
              | "up" `isPrefixOf` head x = f xs h (v - read (last x) :: Int)
              | "down" `isPrefixOf` head x = f xs h (v + read (last x) :: Int)
f [] h v = h * v

f2 :: [[String]] -> Int -> Int -> Int -> Int
f2 (x:xs) h v aim   | "forward" `isPrefixOf` head x = f2 xs (h + read (last x)  :: Int) (v + (aim * read (last x) :: Int)) aim
                    | "backward" `isPrefixOf` head x = f2 xs (h + read (last x) :: Int) v aim
                    | "up" `isPrefixOf` head x = f2 xs h v (aim - read (last x) :: Int) 
                    | "down" `isPrefixOf` head x = f2 xs h v (aim + read (last x) :: Int)
f2 [] h v aim = h * v