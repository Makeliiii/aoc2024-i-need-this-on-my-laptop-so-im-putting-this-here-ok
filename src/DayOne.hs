module DayOne (day_one) where

import Prelude
import Data.List.Split (splitOn)
import Data.List (sort)
import qualified Data.Map as Map

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

create_map :: (Read a, Num a, Ord a) => [a] -> Map.Map a Int
create_map xs = Map.fromList zip_list
  where
    zip_list = zip xs $ repeat 0

update_map :: (Read k, Num k, Ord k) => k -> Map.Map k Int -> Map.Map k Int
update_map = Map.update plus_one
  where
    plus_one = Just . (+) 1

calculate_counts_map :: (Read k, Num k, Ord k) => [k] -> Map.Map k Int -> Map.Map k Int
calculate_counts_map xs counts_map = foldl (flip update_map) counts_map xs

calculate_similarity :: (Int, Int) -> Int
calculate_similarity (x, y) = x * y

calculate_similarity_score :: [(Int, Int)] -> Int
calculate_similarity_score = sum . map calculate_similarity

day_one :: IO ()
day_one = do
  content <- readFile "inputs/d1.txt" :: IO String
  let (xs, ys) = unzip_to_lists content :: ([Int], [Int])
  let distance_sum = sort_and_calculate_distance (xs, ys) :: Int
  print "PART I:"
  print distance_sum
  let counts_map = create_map xs
  let counts = Map.toList (calculate_counts_map ys counts_map)
  let similarity_score = calculate_similarity_score counts
  print "PART II:"
  print similarity_score

