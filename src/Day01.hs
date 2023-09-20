module Day01 (main) where

import System.Environment (getArgs)

main :: IO ()
main = do
  let withDefault a = case a of [] -> "inputs/day01.txt"; x:_ -> x
  text <- getArgs >>= readFile . withDefault
  let nums  = toNums text
      p1    = zip nums (tail nums ++ [head nums])
      p2    = zip nums (cutDeck nums)
  putStrLn $ "Part 1: " ++ show (solve p1)
  putStrLn $ "Part 2: " ++ show (solve p2)

toNums :: [Char] -> [Int]
toNums []        = []
toNums ('\n':xs) = toNums xs
toNums (x   :xs) = read [x] : toNums xs

-- Cut the list in half and return the second half ++ the first half
cutDeck :: [a] -> [a]
cutDeck xs = drop s xs ++ take s xs
  where s = length xs `div` 2

solve :: [(Int, Int)] -> Int
solve [] = 0
solve ((a, b):xs)
  | a == b    = a + solve xs
  | otherwise = solve xs
