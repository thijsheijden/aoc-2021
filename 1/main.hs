main :: IO()
main = do
  -- Load input file
  s <- readFile "input.txt"

  -- Convert strings to ints  
  let depthMeasurements = map (\x -> read x :: Int) (lines s)



  print (f depthMeasurements 0)

f :: [Int] -> Int -> Int
f (x:xs:xss) increases  | x < xs = f (xs:xss) (increases + 1)
                        | x > xs = f (xs:xss) increases
f (x:[]) increases = increases