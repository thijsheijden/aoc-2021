import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails)

main :: IO()
main = do
  -- Load input file
  s <- readFile "input.txt"

  -- Convert strings to ints  
  let depthMeasurements = map (\x -> read x :: Int) (lines s)

  -- First half of puzzle
  print (f depthMeasurements 0)

  -- Second half of puzzle
  print (f2 depthMeasurements)

f :: [Int] -> Int -> Int
f (x:xs:xss) increases  | x < xs = f (xs:xss) (increases + 1)
                        | otherwise = f (xs:xss) increases
f (x:[]) increases = increases

f2 :: [Int] -> Int
f2 m = f (map sum $ window m) 0
  where
    window = foldr (zipWith (:)) (repeat []) . take 3 . tails
