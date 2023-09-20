module Day02 (main) where

import System.Environment (getArgs)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  let withDefault a = case a of [] -> "inputs/day02.txt"; x:_ -> x
  text <- getArgs >>= readFile . withDefault
  let ls = map parseLine (lines text)
  putStrLn $ "Part 1: " ++ show (sum (map p1Line ls))
  putStrLn $ "Part 2: " ++ show (sum (map p2Line ls))

parseLine :: String -> [Int]
parseLine line = map read $ splitOn "\t" line

p1Line :: [Int] -> Int
p1Line xs = maximum xs - minimum xs

p2Line :: [Int] -> Int
p2Line xs = head [ a `div` b | a <- xs, b <- xs, a /= b && a `mod` b == 0]
