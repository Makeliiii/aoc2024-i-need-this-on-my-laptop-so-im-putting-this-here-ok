module DayOne (day_one) where

import Prelude
import Data.List.Split (splitOn)
import Data.List (sort)


calculate_distance :: Num a => a -> a -> a
calculate_distance x y = abs value
  where
    value = x - y

calculate_distances :: Num a => [a] -> [a] -> [a]
calculate_distances [] _ = []
calculate_distances _ [] = []
calculate_distances (x:xs) (y:ys) = calculate_distance x y : calculate_distances xs ys

calculate_distances_sum :: Num a => [a] -> a
calculate_distances_sum = sum

split_to_tuple :: (Read a, Num a) => String -> (a, a)
split_to_tuple xs = tuple $ splitOn "   " xs

tuple :: (Read a, Num a) => [String] -> (a, a)
tuple xs = (read $ head xs, read $ last xs)

unzip_to_lists :: (Read a, Num a) => String -> ([a], [a])
unzip_to_lists = unzip . map split_to_tuple . lines

sort_and_calculate_distance :: (Read a, Num a, Ord a) => ([a], [a]) -> a
sort_and_calculate_distance (xs, ys) = calculate_distances_sum $ calculate_distances x y
  where
    x = sort xs
    y = sort ys

day_one :: IO ()
day_one = do
  content <- readFile "inputs/d1.txt" :: IO String
  let distances = unzip_to_lists content :: ([Int], [Int])
  let distance_sum = sort_and_calculate_distance distances :: Int
  print distance_sum
