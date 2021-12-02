import Data.List

main :: IO()
main = do
  input <- readFile "input.txt"
  
  -- First half
  let x = mul $ foldl (\t v -> f (head v) (read $ last v :: Int) t) (0, 0) (map words $ lines input)

  -- Second half
  let x2 = mul2 $ foldl (\t v -> f2 (head v) (read $ last v :: Int) t) (0, 0, 0) (map words $ lines input)

  print x
  print x2

-- First half
f :: String -> Int -> (Int, Int) -> (Int, Int)
f "forward"   d (h, v) = (h + d, v)
f "backward"  d (h, v) = (h - d, v)
f "up"        d (h, v) = (h, v - d)
f "down"      d (h, v) = (h, v + d)

-- Second half
f2 :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
f2 "forward"  d (h, v, aim) = (h + d, v + aim * d, aim)
f2 "backward" d (h, v, aim) = (h - d, v, aim)
f2 "up"       d (h, v, aim) = (h, v, aim - d)
f2 "down"     d (h, v, aim) = (h, v, aim + d)

mul2 :: (Int, Int, Int) -> Int
mul2 (h, v, _) = h * v

mul :: (Int, Int) -> Int
mul (h, v) = h * v