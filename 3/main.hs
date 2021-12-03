import Data.Char


main :: IO ()
main = do
  l <- readFile "input.txt"
  
  -- Load in the list of numbers, each number is read in as the list of bits [[0, 1, 0, 1, 0], ...]
  let numbers = map (\l -> map digitToInt $ head l) . map words $ lines l

  -- Count how many 1 bits there are at every position
  let bitCount = foldl (\count v -> zipWith (+) count v) (take (length . head $ numbers) (repeat 0 :: [Int])) numbers

  -- For every bit, determine if 1 or 0 is dominant (more than 50%)
  let half = length numbers `div` 2
  let result = foldr (\b s -> if b > half then 1:s else 0:s) ([]::[Int]) bitCount

  let gamma = binaryToDecimal result
  let epsilon = binaryToDecimal $ map (\b -> if b == 1 then 0 else 1) result

  print $ gamma * epsilon

-- Convert a list of bits to decimal number
binaryToDecimal :: [Int] -> Int
binaryToDecimal = foldl (\b -> (+) (2*b)) 0